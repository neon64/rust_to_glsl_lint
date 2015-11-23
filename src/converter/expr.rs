use rustc_front::hir;
use rustc_front::print::pprust;
use syntax::ast;
use syntax::ptr::P;
use rustc::middle::ty;
use std::ops::Deref;

use converter::config::VARIABLE_PREFIX;
use converter::{ConversionError, Context, declare_type, declare_function, convert_type_name};
use structure::Space;
use util::Indent;

/// Compiles a block of Rust code into GLSL
pub fn block_to_glsl(context: &mut Context, tcx: &ty::ctxt, indent: &mut Indent, block: &P<hir::Block>, allow_expr: bool) -> Result<String, ConversionError> {
    let mut result = String::new();

    indent.increment();

    for stmt in &block.stmts {
        result.push_str(&indent.begin_line());
        let text = try!(statement_to_glsl(context, tcx, indent, stmt));
        result.push_str(&text);
    }

    if let Some(ref expr) = block.expr {
        if !allow_expr {
            return Err(ConversionError::ExpressionsNotAllowedInBlock(expr.span));
        }

        result.push_str(&*try!(return_to_glsl(context, tcx, indent, expr)))
    }

    indent.decrement();

    Ok(result)
}

fn return_to_glsl(context: &mut Context, tcx: &ty::ctxt, indent: &mut Indent, expr: &P<hir::Expr>) -> Result<String, ConversionError> {
    let mut result = String::new();
    result.push_str(&indent.begin_line());
    result.push_str("return ");
    let text = try!(expression_to_glsl(context, tcx, indent, expr, true));
    result.push_str(&text);
    result.push_str(";");
    Ok(result)
}

/// Turns a Stmt into a GLSL expression.
fn statement_to_glsl(context: &mut Context, tcx: &ty::ctxt, indent: &mut Indent, stmt: &P<hir::Stmt>) -> Result<String, ConversionError> {
    match stmt.node {
        hir::StmtDecl(ref decl, _) => declaration_statement_to_glsl(context, tcx, indent, decl),
        hir::StmtExpr(ref expr, _) => expression_statement_to_glsl(context, tcx, indent, expr),
        hir::StmtSemi(ref expr, _) => expression_statement_to_glsl(context, tcx, indent, expr)
    }
}

fn declaration_statement_to_glsl(context: &mut Context, tcx: &ty::ctxt, indent: &mut Indent, decl: &P<hir::Decl>) -> Result<String, ConversionError> {
    match decl.node {
        hir::Decl_::DeclLocal(ref dec) => {
            let mut result = String::new();

            let ty = tcx.node_id_to_type(dec.id);
            result.push_str(try!(try!(declare_type(context, tcx, ty, decl.span)).as_str(Space::NotArg)).deref());

            // identifier
            match dec.pat.node {
                hir::Pat_::PatIdent(_, ref ident, _) => {
                    result.push_str(" ");
                    result.push_str(VARIABLE_PREFIX);
                    result.push_str(&ident.node.name.as_str());
                },
                _ => {
                    return Err(ConversionError::Unimplemented(dec.span));
                }
            }

            // initializer
            if let Some(ref expr) = dec.init {
                result.push_str(" = ");
                result.push_str(try!(expression_to_glsl(context, tcx, indent, expr, true)).deref());
            }

            result.push_str(";");

            Ok(result)
        }
        hir::Decl_::DeclItem(_) => {
            Err(ConversionError::Unimplemented(decl.span))
        }
    }

}

/// Turns an Expr used in the context of a Stmt into a GLSL expression.
fn expression_statement_to_glsl(context: &mut Context, tcx: &ty::ctxt, indent: &mut Indent, expr: &P<hir::Expr>) -> Result<String, ConversionError> {
    match expr.node {
        hir::ExprIf(ref condition, ref block, ref el) => {
            let condition = try!(expression_to_glsl(context, tcx, indent, condition, true));
            let block = try!(block_to_glsl(context, tcx, indent, block, false));

            if let &Some(ref expr) = el {
                Ok(format!("if ({}) {{ {}{}}} else {}", condition, block, indent.begin_line(), try!(expression_statement_to_glsl(context, tcx, indent, expr))))
            } else {
                Ok(format!("if ({}) {{ {}{}}}", condition, block, indent.begin_line()))
            }
        },
        hir::ExprWhile(ref cond, ref block, None) => {
            let cond = try!(expression_to_glsl(context, tcx, indent, cond, true));
            let block = try!(block_to_glsl(context, tcx, indent, block, false));

            Ok(format!("while ({}) {{ {} }}", cond, block))
        },
        hir::ExprLoop(ref block, None) => {
            let block = try!(block_to_glsl(context, tcx, indent, block, false));
            Ok(format!("while (true) {{ {} }}", block))
        },
        hir::ExprBlock(ref block) => {
            let block = try!(block_to_glsl(context, tcx, indent, block, false));
            Ok(format!("{{ {}{}}}", block, indent.begin_line()))
        },
        hir::ExprRet(Some(ref expr)) => return_to_glsl(context, tcx, indent, expr),
        hir::ExprRet(None) => Ok("return;".to_owned()),
        hir::ExprBreak(None) => Ok("break;".to_owned()),
        hir::ExprAssign(ref left, ref right) => {
            let left_str = try!(expression_to_glsl(context, tcx, indent, left, true));
            let right_str = try!(expression_to_glsl(context, tcx, indent, right, true));
            Ok(format!("{} = {};", left_str, right_str))
        },
        _ => Err(ConversionError::Unimplemented(expr.span))
    }
}

// =========================================
//
//               EXPRESSION
//
// =========================================

/// Turns an Expr into a GLSL expression.
fn expression_to_glsl(context: &mut Context, tcx: &ty::ctxt, indent: &mut Indent, expr: &P<hir::Expr>, add_path_prefix: bool) -> Result<String, ConversionError> {
    match expr.node {
        //hir::ExprCast(ref e, ref t) => Ok(format!("{}({})", try!(declare_type(context, tcx, t)).as_str(), try!(expression_to_glsl(context, tcx, indent, e, add_path_prefix)))),
        hir::ExprLit(ref lit) => literal_to_glsl(context, lit),
        hir::ExprRet(Some(ref expr)) => {
            return_to_glsl(context, tcx, indent, expr)
        },
        hir::ExprRet(None) => Ok(format!("return;")),
        hir::ExprBreak(None) => Ok(format!("break;")),
        hir::ExprCall(ref f, ref params) => {
            let fn_ty = tcx.node_id_to_type(f.id);
            //let fn_expr = try!(expression_to_glsl(context, tcx, indent, f, false));

            let name = try!(declare_function(context, tcx, fn_ty, f.span));

            let mut args = Vec::new();
            for p in params {
                args.push(try!(expression_to_glsl(context, tcx, indent, p, true)));
            }

            Ok(format!("{}({})", name, args.join(", ")))
        },
        hir::ExprPath(_, ref path) => {
            let name = convert_type_name(&pprust::path_to_string(path));
            Ok(if add_path_prefix { format!("{}{}", VARIABLE_PREFIX, name) } else { name })
            /*let path_str = path_to_glsl(path);
            match &path_str[..] {
                "Vec4::new" => Ok("vec4".to_owned()),
                "Vec3::new" => Ok("vec3".to_owned()),
                "Vec2::new" => Ok("vec2".to_owned()),
                _ => {
                    if path.global || path.segments.len() != 1 {
                        return Err(Error::UnexpectedConstruct(expr.span, "global or namespaced path expressions unimplemented".to_owned()));
                    }

                    let mut result = String::new();
                    if add_path_prefix {
                        result.push_str(VARIABLE_PREFIX);
                    }
                    result.push_str(&path.segments[0].identifier.to_owned());

                    Ok(result)
                }
            }*/
        },
        hir::ExprBinary(op, ref left, ref right) => {
            let op = match op.node {
                hir::BiAdd => "+",
                hir::BiSub => "-",
                hir::BiMul => "*",
                hir::BiDiv => "/",
                hir::BiRem => "%",
                hir::BiAnd => "&&",
                hir::BiOr => "||",
                hir::BiBitXor => "^",
                hir::BiBitAnd => "&",
                hir::BiBitOr => "|",
                hir::BiShl => "<<",
                hir::BiShr => ">>",
                hir::BiEq => "==",
                hir::BiLt => "<",
                hir::BiLe => "<=",
                hir::BiNe => "!=",
                hir::BiGe => ">=",
                hir::BiGt => ">",
            };

            let left = try!(expression_to_glsl(context, tcx, indent, left, add_path_prefix));
            let right = try!(expression_to_glsl(context, tcx, indent, right, add_path_prefix));

            Ok(format!("({} {} {})", left, op, right))
        },
        hir::ExprUnary(op, ref val) => {
            let op = match op {
                hir::UnNot => "!",
                hir::UnNeg => "-",
                _ => {
                    return Err(ConversionError::UnsupportedOperation(expr.span));
                }
            };

            let val = try!(expression_to_glsl(context, tcx, indent, val, add_path_prefix));

            Ok(format!("({}{})", op, val))
        },
        hir::ExprStruct(_, ref fields, _) => {
            let ty = tcx.node_id_to_type(expr.id);

            let mut result = String::new();

            result.push_str(try!(try!(declare_type(context, tcx, ty, expr.span)).as_str(Space::NotArg)).deref());
            result.push_str("(");

            indent.increment();

            if let ty::TyStruct(ref def, _) = ty.sty {
                let fields: Result<Vec<_>, _> = def.all_fields().map(|field| {
                    let ident = field.name.as_str().to_string();

                    for field_value in fields {
                        if ident.as_str() == field_value.name.node.as_str() {
                            let mut r = indent.begin_line();
                            r.push_str(&*try!(expression_to_glsl(context, tcx, indent, &field_value.expr, true)));
                            return Ok(r);
                        }
                    }
                    panic!("field not provided")
                }).collect();
                match fields {
                    Ok(fields) => result.push_str(&fields.join(",")),
                    Err(err) => return Err(err)
                }
            }

            indent.decrement();

            result.push_str(&indent.begin_line());
            result.push_str(")");

            Ok(result)
        },
        hir::ExprField(ref expr, ref ident) => {
            let expr_str = try!(expression_to_glsl(context, tcx, indent, expr, add_path_prefix));
            let ident_str = ident.node.to_string();

            let check_for_swizzle = regex!(r"^[xyzwrgbastuv]+$");
            let prefix = if check_for_swizzle.is_match(&ident_str) { "" } else { VARIABLE_PREFIX }; // swizzles have no prefix

            let result = format!("{}.{}{}", expr_str, prefix, ident_str);

            Ok(result)
            /*match identifier_mappings.get(&result) {
                Some(replacement) => Ok(replacement.clone()),
                None => Ok(result)
            }*/
        },
        _ => Err(ConversionError::Unimplemented(expr.span)),
    }
}

/// Turns a literal into a GLSL expression.
fn literal_to_glsl(_: &mut Context, lit: &P<ast::Lit>) -> Result<String, ConversionError> {
    match lit.node {
        ast::LitInt(val, ast::SignedIntLit(_, ast::Minus)) => Ok(format!("-{}", val)),
        ast::LitInt(val, _) => Ok(format!("{}", val)),
        ast::LitFloat(ref s, _) => Ok(s.deref().to_owned()),
        ast::LitFloatUnsuffixed(ref s) => {
            // transforms 1. to 1f
            let lit = s.deref().to_owned();
            let regex = regex!(r"\.$");
            let transformed = regex.replace_all(&lit, ".0");

            Ok(transformed)
        },
        ast::LitBool(val) => Ok(if val { "true" } else { "false" }.to_owned()),
        ast::LitStr(ref val, _) => Err(ConversionError::UnsupportedLiteral(lit.span, val.deref().to_owned())),
        ast::LitByteStr(ref val) => Err(ConversionError::UnsupportedLiteral(lit.span, format!("{:?}", val.deref()))),
        ast::LitByte(val) => Err(ConversionError::UnsupportedLiteral(lit.span, val.to_string())),
        ast::LitChar(val) => Err(ConversionError::UnsupportedLiteral(lit.span, val.to_string())),
    }
}