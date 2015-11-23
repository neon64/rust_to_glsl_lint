use rustc_front::hir;
use rustc::front::map::Node::NodeItem;
use syntax::codemap::Span;
use syntax::ast;
use rustc::middle::ty;
use std::collections::HashMap;

mod config;
mod expr;
pub mod interface;

use structure::{Shader, Function, FunctionName, Arg, Struct, StructField, Type, CodeGenerationError};
use converter::expr::{block_to_glsl};
use converter::config::VARIABLE_PREFIX;
use attribute::{get_attribute_name_value, get_attribute_list_named_value, get_struct_field_attrs};
use util::Indent;

pub struct Context {
    pub shader: Shader,
    pub built_in_types: HashMap<&'static str, &'static str>,
    pub built_in_functions: HashMap<&'static str, &'static str>
}

#[derive(Debug)]
pub enum ConversionError {
    /// The type can't be represented in GLSL
    UnsupportedType(Span),
    /// The function never returns
    DivergingFunction(Span),
    /// The lint pass can't access the AST for functions outside of the current crate
    ExternalFunction(Span),
    /// Certain blocks (if/while etc.) can't have expressions
    ExpressionsNotAllowedInBlock(Span),
    /// The given feature is unimplemented
    Unimplemented(Span),
    /// The literal can't be converted
    UnsupportedLiteral(Span, String),
    /// The operation (+ - * / % []) is unavailable in GLSL
    UnsupportedOperation(Span),
    /// There was an error when 'stringifying' something
    CodeGenerationError(CodeGenerationError)
}

impl From<CodeGenerationError> for ConversionError {
    fn from(e: CodeGenerationError) -> Self {
        ConversionError::CodeGenerationError(e)
    }
}

/// Converts the given type to GLSL
pub fn declare_type<'a>(context: &mut Context, tcx: &ty::ctxt<'a>, ty: &ty::TyS<'a>, span: Span) -> Result<Type, ConversionError> {
    match ty.sty {
        ty::TyBool => Ok(Type::BuiltIn("bool")),
        ty::TyInt(_) => Ok(Type::BuiltIn("int")),
        ty::TyUint(_) => Ok(Type::BuiltIn("uint")),
        ty::TyFloat(ty) => match ty {
            ast::TyF32 => Ok(Type::BuiltIn("float")),
            ast::TyF64 => Ok(if context.shader.required_glsl_version.is_greater_than_or_equal_to(4, 0) { Type::BuiltIn("double") } else { Type::BuiltIn("float") })
        },
        ty::TyStruct(ref def, ref subst) => {
            // get the full name of the type including namespaces and generics
            let full_name = format!("{:?}", ty);
            if let Some(built_in) = context.built_in_types.get(&*full_name) {
                return Ok(Type::BuiltIn(built_in))
            }
            let mut fields = Vec::new();
            for field in &def.struct_variant().fields {
                let attrs = get_struct_field_attrs(tcx, def.did, field.did);
                let built_in = if let Some(ref attrs) = attrs {
                    (get_attribute_list_named_value(attrs, "glsl_repr", "input"), get_attribute_list_named_value(attrs, "glsl_repr", "output"))
                } else {
                    (None, None)
                };
                let ty = try!(declare_type(context, tcx, field.ty(tcx, subst), span));
                fields.push(StructField {
                    ty: ty,
                    name: format!("{}{}", VARIABLE_PREFIX, field.name.as_str()),
                    built_in: built_in
                });
            }

            let struct_name = convert_type_name(&full_name);
            let struct_def = Struct { fields: fields };

            context.shader.structs.insert(struct_name.clone(), struct_def);
            Ok(Type::Struct(struct_name))
        },
        ty::TyRef(_, ref ty) => {
            let inner = Box::new(try!(declare_type(context, tcx, ty.ty, span)));
            match ty.mutbl {
                hir::MutImmutable => Ok(Type::Ref(inner)),
                hir::MutMutable => Ok(Type::RefMut(inner))
            }
        }
        // TyTuple
        ty::TyEnum(ref def, _) => {
            let span = tcx.map.def_id_span(def.did, span);
            Err(ConversionError::UnsupportedType(span))
        }
        _ => { // TyChar, TyBox, TyStr, TySlice, TyRawPtr, TyBareFn, TyRef, TyTrait, TyClosure
            Err(ConversionError::UnsupportedType(span))
        }
    }
}

/// Converts a function into GLSL
pub fn declare_function<'a>(context: &mut Context, tcx: &ty::ctxt<'a>, ty: &ty::TyS<'a>, span: Span) -> Result<FunctionName, ConversionError> {
    match ty.sty {
        ty::TyBareFn(Some(did), def) => {
            // check for built-in built-ins
            if let Some(built_in) = context.built_in_functions.get(&*tcx.item_path_str(did)) {
                return Ok((*built_in).to_owned())
            }

            // check for custom built-ins (activated by an attribute)
            let attrs = tcx.get_attrs(did);
            if let Some(built_in) = get_attribute_name_value(&*attrs, "glsl_repr") {
                return Ok(built_in)
            }

            // get the AST
            let signature = def.sig.skip_binder();
            let (fn_decl, fn_block) = match tcx.map.get_if_local(did) {
                Some(ast) => if let NodeItem(i) = ast {
                    if let hir::ItemFn(ref decl, _, _, _, _, ref block) = i.node { (decl, block) } else { panic!("must be a function") }
                } else {
                    panic!("must be an item")
                },
                None => return Err(ConversionError::ExternalFunction(span))
            };

            // arguments
            let mut args = Vec::new();
            for (ty, arg) in signature.inputs.iter().zip(&fn_decl.inputs) {
                let ty = try!(declare_type(context, tcx, ty, arg.ty.span));
                let name = match arg.pat.node {
                    hir::PatIdent(_, ref ident, _) => {
                        format!("{}{}", VARIABLE_PREFIX, &ident.node.to_owned())
                    },
                    _ => {
                        return Err(ConversionError::Unimplemented(arg.pat.span));
                    }
                };
                args.push(Arg::new(ty, name));
            }

            // return type
            let return_ty = match signature.output {
                ty::FnConverging(ty) => try!(declare_type(context, tcx, ty, fn_decl.output.span())),
                ty::FnDiverging => return Err(ConversionError::DivergingFunction(fn_decl.output.span()))
            };

            // get the full name of the function including namespaces
            // at the moment it doesn't handle generics
            let name = tcx.item_path_str(did);

            let mut indent = Indent::new();
            let body = block_to_glsl(context, tcx, &mut indent, fn_block, true).unwrap();

            let function = Function {
                args: args,
                return_ty: return_ty,
                body: body
            };

            context.shader.functions.insert(name.clone(), function);

            Ok(name)
        },
        ty::TyBareFn(None, _) => Err(ConversionError::ExternalFunction(span)),
        _ => Err(ConversionError::UnsupportedType(span))
    }
}

fn convert_type_name(name: &str) -> String {
    let namespace_regex = regex!(r"::");
    let less_than_greater_than_regex = regex!(r"[<>]");
    less_than_greater_than_regex.replace_all(&namespace_regex.replace_all(name, "_"), "__")
}
