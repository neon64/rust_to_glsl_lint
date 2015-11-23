use linked_hash_map::LinkedHashMap;
use std::collections::HashMap;

use converter::Context;
use structure::*;
use converter::config::VARIABLE_PREFIX;
use util::Indent;

#[derive(Debug)]
pub enum WrapperFunctionGenerationError {
    FunctionNotFound(String),
    StructNotFound(String),
    UnknownBuiltInVariable(String),
    BuiltInVariableTypeMismatch(String, Type, Type),
    ReturnRef(Type),
    CodeGenerationError(CodeGenerationError)
}

impl From<CodeGenerationError> for WrapperFunctionGenerationError {
    fn from(e: CodeGenerationError) -> Self {
        WrapperFunctionGenerationError::CodeGenerationError(e)
    }
}

pub fn generate_main_function(context: &mut Context, name: String) -> Result<(), WrapperFunctionGenerationError> {
    let def = if let Some(ref def) = context.shader.functions.get(&name) {
        let mut in_count_map = HashMap::new();
        let mut indent = Indent::new();
        indent.increment();
        let mut body = String::new();
        body.push_str(&indent.begin_line());
        body.push_str(&format!("{} result = {}(", try!(def.return_ty.as_str(Space::NotArg)), name));

        // loop through all the arguments and declare them as global inputs
        {
            let structs = &context.shader.structs;
            let blocks = &mut context.shader.interface_blocks;
            let globals = &mut context.shader.globals;
            let ty = context.shader.ty;
            let fields: Result<Vec<String>, WrapperFunctionGenerationError> = def.args.iter().map(|arg| {
                let arg_ty_str = try!(arg.ty.as_str(Space::Serialize));
                let entry = in_count_map.entry(arg_ty_str.clone()).or_insert(0);
                let decl = try!(declare_type_global(
                    ty,
                    structs, globals, blocks,
                    Qualifier::Input,
                    &arg.ty,
                    &format!("{}_{}", arg_ty_str, entry),
                    Mode::Input
                ));
                *entry += 1;
                Ok(decl)
            }).collect();
            body.push_str(&try!(fields).join(", "));
            body.push_str(");");
        }

        // declare the global outputs
        {
            let return_ty_str = try!(def.return_ty.as_str(Space::Serialize));
            let entry = in_count_map.entry(return_ty_str.clone()).or_insert(0);
            let r = try!(declare_type_global(
                context.shader.ty,
                &context.shader.structs,
                &mut context.shader.globals,
                &mut context.shader.interface_blocks,
                Qualifier::Output,
                &def.return_ty,
                &format!("{}_{}", return_ty_str, entry),
                Mode::Output(&indent, "result")
            ));
            body.push_str(&r);
        }

        // construct the main function
        Function {
            return_ty: Type::BuiltIn("void"),
            args: Vec::new(),
            body: body
        }
    } else {
        return Err(WrapperFunctionGenerationError::FunctionNotFound(name))
    };

    context.shader.functions.insert("main".to_owned(), def);

    Ok(())
}

// declares a global type
fn declare_type_global(shader: ShaderType, structs: &LinkedHashMap<String, Struct>, globals: &mut LinkedHashMap<String, Global>, interface_blocks: &mut LinkedHashMap<String, InterfaceBlock>, qualifier: Qualifier, ty: &Type, global_name: &str, mode: Mode) -> Result<String, WrapperFunctionGenerationError> {
    // handle uniforms
    if let &Type::Ref(ref ty) = ty {
        match qualifier {
            Qualifier::Input | Qualifier::Uniform => {
                let name = try!(declare_uniform(interface_blocks, ty, &format!("u_{}", global_name)));
                return Ok(assign_global(mode, &name, None));
            },
            Qualifier::Output => return Err(WrapperFunctionGenerationError::ReturnRef(*ty.clone()))
        }
    }

    let global_name = format!("{}_{}", match mode { Mode::Input => shader.get_input_prefix(), Mode::Output(_, _) => shader.get_output_prefix() }, global_name);

    declare_type_destructured(structs, globals, qualifier, ty, &global_name, String::new(), mode)
}

fn assign_global(mode: Mode, full_name: &str, field_name: Option<&str>) -> String {
    match mode {
        Mode::Input => full_name.to_owned(),
        Mode::Output(indent, assign_from) => {
            let assign = if let Some(field_name) = field_name {
                format!("{name}.{field}", name = assign_from, field = field_name)
            } else {
                format!("{}", assign_from)
            };
            format!("{line}{full_name} = {assign};", line = &indent.begin_line(), full_name = full_name, assign = assign)
        }
    }
}

// declare the type not as a whole but as a collection of simple fields
fn declare_type_destructured(structs: &LinkedHashMap<String, Struct>, globals: &mut LinkedHashMap<String, Global>, qualifier: Qualifier, ty: &Type, global_name: &str, local_name: String, mode: Mode) -> Result<String, WrapperFunctionGenerationError> {
    let mut result = String::new();
    match *ty {
        Type::BuiltIn("void") => {},
        Type::BuiltIn(_) => {
            let full_name = format!("{}_{}", global_name, local_name);
            result.push_str(&assign_global(mode, &full_name, Some(&local_name)));
            globals.insert(full_name, Global { ty: ty.clone(), qualifier: qualifier });
        },
        // recurse through each struct field
        Type::Struct(ref struct_name) => {
            if let Mode::Input = mode {
                result.push_str(struct_name);
                result.push_str("(");
            }

            if let Some(ref def) = structs.get(struct_name)  {
                let fields: Result<Vec<String>, WrapperFunctionGenerationError> = def.fields.iter()
                    .map(|field| {
                        let local = if local_name == "" { field.name.clone() } else { format!("{}.{}", local_name, field.name) };
                        match mode {
                            Mode::Input => if let Some(ref built_in) = field.built_in.0 {
                                let built_in = try!(built_in_to_glsl(built_in, &field.ty));
                                Ok(assign_global(mode, &built_in, Some(&local)))
                            } else {
                                declare_type_destructured(structs, globals, qualifier, &field.ty, global_name, local, mode)
                            },
                            Mode::Output(_, _) => if let Some(ref built_in) = field.built_in.1 {
                                let built_in = try!(built_in_to_glsl(built_in, &field.ty));
                                let mut assign = assign_global(mode, &built_in, Some(&local));
                                if let Mode::Output(_, _) = mode {
                                    assign.push_str(&*try!(declare_type_destructured(structs, globals, qualifier, &field.ty, global_name, local, mode)));
                                }
                                Ok(assign)
                            } else {
                                declare_type_destructured(structs, globals, qualifier, &field.ty, global_name, local, mode)
                            }
                        }
                    })
                    .collect();
                let joined = match mode {
                    Mode::Input => try!(fields).join(", "),
                    Mode::Output(_, _) => try!(fields).join("")
                };
                result.push_str(&joined);
            } else {
                return Err(WrapperFunctionGenerationError::StructNotFound(struct_name.clone()));
            }

            if let Mode::Input = mode {
                result.push_str(")");
            }
        },
        Type::Ref(ref ty) => match qualifier {
            Qualifier::Output => return Err(WrapperFunctionGenerationError::ReturnRef(*ty.clone())),
            _ => return declare_type_destructured(structs, globals, qualifier, ty, global_name, local_name, mode)
        },
        _ => {
            unimplemented!()
        }
    }

    Ok(result)
}

fn declare_uniform(interface_blocks: &mut LinkedHashMap<String, InterfaceBlock>, ty: &Type, global_name: &str) -> Result<String, WrapperFunctionGenerationError> {
    let field_name = format!("{}{}", VARIABLE_PREFIX, global_name);
    interface_blocks.insert(global_name.to_owned(), InterfaceBlock {
        layout: Layout::Std140,
        storage_type: StorageType::Uniform,
        fields: vec![StructField {
            ty: ty.clone(),
            name: field_name.clone(),
            built_in: (None, None)
        }]
    });
    Ok(field_name)
}

pub fn built_in_to_glsl(name: &str, field: &Type) -> Result<String, WrapperFunctionGenerationError> {
    match name {
        "vertex-position" => if let Type::BuiltIn("vec4") = *field {
            Ok("gl_Position".to_owned())
        } else {
            Err(WrapperFunctionGenerationError::BuiltInVariableTypeMismatch("gl_Position".to_owned(), field.clone(), Type::BuiltIn("vec4")))
        },
        _ => Err(WrapperFunctionGenerationError::UnknownBuiltInVariable(name.to_owned()))
    }
}

#[derive(Copy, Clone, Debug)]
enum Mode<'a> {
    Input,
    Output(&'a Indent, &'a str)
}