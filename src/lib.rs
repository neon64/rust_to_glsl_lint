#![feature(plugin_registrar)]
#![feature(rustc_private)]
#![feature(plugin)]
#![plugin(regex_macros)]
#![feature(convert)]

#[macro_use]
extern crate rustc;
extern crate rustc_front;
extern crate syntax;
extern crate regex;
extern crate term;
#[macro_use]
extern crate maplit;
extern crate itertools;
extern crate linked_hash_map;

mod attribute;
mod structure;
mod converter;
mod util;
#[cfg(feature="optimize_shaders")]
extern crate glsl_optimizer;
#[cfg(feature="optimize_shaders")]
mod optimize;

use syntax::codemap::Span;
use syntax::ast;
use syntax::feature_gate::AttributeType;
use rustc_front::hir;
use rustc::session::Session;
use rustc::lint::{LateContext, LintArray, LintPass, LateLintPass};
use std::collections::HashMap;
use std::path::Path;
use std::fs::File;
use std::io::Write;
use std::str::FromStr;

use structure::{Shader, ShaderType, GlslVersion, Space};
use converter::{declare_function, Context, ConversionError};
use converter::interface::{WrapperFunctionGenerationError, generate_main_function};
use converter::interface::WrapperFunctionGenerationError::*;
#[cfg(feature="optimize_shaders")]
use optimize::optimize_shader;

declare_lint! {
    pub RUST_TO_GLSL_CONVERTER, Warn,
    "Technically not a lint, it converts a rust function into GLSL code"
}

#[derive(Copy,Clone)]
pub struct Pass;

impl LintPass for Pass {
    fn get_lints(&self) -> LintArray {
        lint_array!(RUST_TO_GLSL_CONVERTER)
    }
}

impl LateLintPass for Pass {

    fn check_item(&mut self, cx: &LateContext, item: &hir::Item) {
        let ty = find_shader_type(&item.attrs, &cx.tcx.sess);
        if let Some(shader_type) = ty {
            // insert check here.
            if let hir::ItemFn(_, _, _, _, _, _) = item.node {
                let shader = Shader::new(shader_type, GlslVersion(3, 3));
                let mut context = Context {
                    shader: shader,
                    built_in_types: get_built_in_types(),
                    built_in_functions: get_built_in_functions()
                };
                let ty = cx.tcx.node_id_to_type(item.id);

                // convert everything
                let name = match declare_function(&mut context, cx.tcx, ty, item.span) {
                    Ok(name) => name,
                    Err(err) => return print_conversion_error(cx.tcx.sess, item.span, err)
                };

                // generates a wrapper around the function, connecting fn inputs and outputs to global GLSL variables
                if let Err(e) = generate_main_function(&mut context, name) {
                    print_wrapper_gen_error(cx.tcx.sess, item.span, e)
                };

                let shader = match optimize_shader(cx.tcx.sess, item.span, shader_type, context.shader.to_glsl().unwrap()) {
                    Ok(shader) => shader,
                    Err(_) => {
                        return
                    }
                };

                let name = cx.tcx.sess.codemap().span_to_filename(item.span);
                let path = Path::new(&name).parent().unwrap().join(&*item.name.as_str()).with_extension("glsl");
                let mut file = File::create(path).unwrap();
                file.write_all(shader.as_bytes()).unwrap();
            } else {
                cx.tcx.sess.span_err(item.span, "#[shader] attribute can only be applied to a function");
            }
        }
    }
}

fn find_shader_type(attrs: &[ast::Attribute], sess: &Session) -> Option<ShaderType> {
    for attr in attrs {
        match attr.node.value.node { // uniform(scope="foo")
            ast::MetaList(ref name, ref items) => if *name == "shader" {
                if let Some(item) = items.first() {
                    if let ast::MetaWord(ref value) = item.node { // scope="foo"
                        if let Ok(ty) = ShaderType::from_str(value) {
                            return Some(ty)
                        } else {
                            sess.span_err(item.span, "invalid shader type for #[shader] attribute");
                        }
                    } else {
                        print_attr_error(sess, attr.span)
                    }
                } else {
                    print_attr_error(sess, attr.span)
                }
            },
            ast::MetaWord(ref name) => if *name == "shader" {
                print_attr_error(sess, attr.span)
            },
            ast::MetaNameValue(ref name, _) => if *name == "shader" {
                print_attr_error(sess, attr.span)
            }
        }
    }
    None
}

fn print_attr_error(sess: &Session, span: Span) {
    sess.span_err(span, "#[shader] attribute should have one argument that specifies the shader type");
    sess.span_help(span, "provide the type of shader like this: #[shader(vertex)] or #[shader(fragment)]");
}

fn print_conversion_error(sess: &Session, outer_span: Span, e: ConversionError) {
    match e {
        ConversionError::UnsupportedType(span) => print_error(sess, span, outer_span, "type is unrepresentable in GLSL"),
        ConversionError::DivergingFunction(span) => print_error(sess, span, outer_span, "diverging functions unrepresentable in GLSL"),
        ConversionError::ExternalFunction(span) => print_error(sess, span, outer_span, "functions outside the current crate can't be converted because their AST is no longer available"),
        ConversionError::UnsupportedLiteral(span, lit) => print_error(sess, span, outer_span, &format!("unsupported literal {}", lit)),
        ConversionError::UnsupportedOperation(span) => print_error(sess, span, outer_span, "unsupported operation"),
        ConversionError::Unimplemented(span) => print_error(sess, span, outer_span, "unimplemented expression"),
        ConversionError::ExpressionsNotAllowedInBlock(span) => print_error(sess, span, outer_span, "expressions are not allowed inside this block"),
        ConversionError::CodeGenerationError(err) => print_error(sess, outer_span, outer_span, &format!("{:?}", err)),
    }
}

fn print_wrapper_gen_error(sess: &Session, span: Span, e: WrapperFunctionGenerationError) {
    match e {
        FunctionNotFound(name) => print_error_plain(sess, span, &format!("function definition for `{}` is missing", name)),
        StructNotFound(name) => print_error_plain(sess, span, &format!("struct definition for `{}` is missing", name)),
        UnknownBuiltInVariable(name) => print_error_plain(sess, span, &format!("unkown built in variable {}", name)),
        BuiltInVariableTypeMismatch(name, found, expected) => print_error_plain(sess, span, &format!("failed to link built in variable `{}`: expected type {} but found {}", name, expected.as_str(Space::NotArg).unwrap(), found.as_str(Space::NotArg).unwrap())),
        ReturnRef(ty) => print_error_plain(sess, span, &format!("cannot return a reference of type {}", ty.as_str(Space::NotArg).unwrap())),
        CodeGenerationError(err) => print_error_plain(sess, span, &format!("{:?}", err)),
    }
}

fn print_error(sess: &Session, span: Span, outer_span: Span, message: &str) {
    sess.span_err(span, message);
    sess.span_note(outer_span, "while converting function to GLSL")
}

fn print_error_plain(sess: &Session, span: Span, message: &str) {
    sess.span_err(span, message);
    sess.note("while converting function to GLSL")
}


fn get_built_in_types() -> HashMap<&'static str, &'static str> {
    hashmap! {
        "na::structs::vec::Vec2<f32>" => "vec2",
        "na::structs::vec::Vec3<f32>" => "vec3",
        "na::structs::vec::Vec4<f32>" => "vec4"
    }
}

fn get_built_in_functions() -> HashMap<&'static str, &'static str> {
    hashmap! {
        "na::structs::vec::Vec2<N>::new" => "vec2",
        "na::structs::vec::Vec3<N>::new" => "vec3",
        "na::structs::vec::Vec4<N>::new" => "vec4"
    }
}

#[cfg(not(feature="optimize_shaders"))]
fn optimize_shader(_: &Session, _: Span, _: ShaderType, source: String) -> Result<String, ()> {
    Ok(source)
}

#[doc(hidden)]
#[plugin_registrar]
pub fn registrar(registry: &mut rustc::plugin::Registry) {
    registry.register_late_lint_pass(Box::new(Pass));
    registry.register_attribute("shader".to_owned(), AttributeType::Whitelisted);
    registry.register_attribute("glsl_repr".to_owned(), AttributeType::Whitelisted);
}
