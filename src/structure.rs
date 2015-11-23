use itertools::Itertools;
use linked_hash_map::LinkedHashMap;
use std::borrow::Cow;
use std::ops::Deref;
use std::str::FromStr;

#[derive(Debug)]
pub enum CodeGenerationError {
    UnsupportedType(&'static str)
}

pub struct Shader {
    pub ty: ShaderType,
    pub required_glsl_version: GlslVersion,
    pub structs: LinkedHashMap<String, Struct>,
    pub interface_blocks: LinkedHashMap<String, InterfaceBlock>,
    pub functions: LinkedHashMap<String, Function>,
    pub globals: LinkedHashMap<String, Global>
}

impl Shader {
    pub fn new(ty: ShaderType, glsl_version: GlslVersion) -> Self {
        Shader {
            ty: ty,
            required_glsl_version: glsl_version,
            structs: LinkedHashMap::new(),
            interface_blocks: LinkedHashMap::new(),
            functions: LinkedHashMap::new(),
            globals: LinkedHashMap::new()
        }
    }

    pub fn to_glsl(&self) -> Result<String, CodeGenerationError> {
        let mut result = String::new();
        result.push_str(&format!("#version {}{}0\n", self.required_glsl_version.0, self.required_glsl_version.1));

        for (name, struct_def) in self.structs.iter() {
            result.push_str(&(try!(struct_def.to_glsl(name)) + "\n"));
        }
        for (name, global) in self.globals.iter() {
            result.push_str(&(try!(global.to_glsl(name)) + "\n"));
        }
        for (name, interface_def) in self.interface_blocks.iter() {
            result.push_str(&(try!(interface_def.to_glsl(name)) + "\n"));
        }
        for (name, fn_def) in self.functions.iter() {
            result.push_str(&(try!(fn_def.to_glsl(name)) + "\n"));
        }

        Ok(result)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ShaderType {
    Vertex,
    Fragment
}

impl FromStr for ShaderType {
    type Err = ();
    fn from_str(message: &str) -> Result<Self, ()> {
        match message {
            "vertex" => Ok(ShaderType::Vertex),
            "fragment" => Ok(ShaderType::Fragment),
            _ => Err(())
        }
    }
}

impl ShaderType {
    pub fn get_input_prefix(&self) -> &'static str {
        match *self {
            ShaderType::Vertex => "vertex_in",
            ShaderType::Fragment => "fragment_in"
        }
    }

     pub fn get_output_prefix(&self) -> &'static str {
        match *self {
            ShaderType::Vertex => "fragment_in",
            ShaderType::Fragment => "fragment_out"
        }
    }
}

/// Represents a version of GLSL.
/// The version is split into a major and a minor version.
pub struct GlslVersion(pub u8, pub u8);

impl GlslVersion {
    pub fn is_greater_than_or_equal_to(&self, major: u8, minor: u8) -> bool {
        self.0 >= major && self.1 >= minor
    }
}

#[derive(Debug)]
pub struct Struct {
    pub fields: Vec<StructField>
}

impl Struct {
    pub fn to_glsl(&self, name: &str) -> Result<String, CodeGenerationError> {
        let mut result = String::new();
        result.push_str("struct ");
        result.push_str(name);
        result.push_str(" {\n");
        for field in &self.fields {
             result.push_str("     ");
             result.push_str(&* try!(field.to_glsl()));
             result.push_str("\n");
        }
        result.push_str("};");
        Ok(result)
    }
}
#[derive(Debug)]
pub struct InterfaceBlock {
    pub layout: Layout,
    pub storage_type: StorageType,
    pub fields: Vec<StructField>
}

impl InterfaceBlock {
    pub fn to_glsl(&self, name: &str) -> Result<String, CodeGenerationError> {
        let mut result = String::new();

        result.push_str(&format!("layout({}) {} ", self.layout.to_glsl(), self.storage_type.to_glsl()));

        result.push_str(name);
        result.push_str(" {\n");
        for field in &self.fields {
             result.push_str("     ");
             result.push_str(&* try!(field.to_glsl()));
             result.push_str("\n");
        }
        result.push_str("};");
        Ok(result)
    }
}

#[derive(Debug)]
pub enum StorageType {
    In, Out, Uniform, Buffer
}

impl StorageType {
    fn to_glsl(&self) -> &str {
        match *self {
            StorageType::In => "in",
            StorageType::Out => "out",
            StorageType::Uniform => "uniform",
            StorageType::Buffer => "buffer"
        }
    }
}

#[derive(Debug)]
pub enum Layout {
    Packed,
    Shared,
    Std140,
    Std430
}

impl Layout {
    fn to_glsl(&self) -> &str {
        match *self {
            Layout::Packed => "packed",
            Layout::Shared => "shared",
            Layout::Std140 => "std140",
            Layout::Std430 => "std430"
        }
    }
}

#[derive(Debug)]
pub struct StructField {
    pub ty: Type,
    pub name: String,
    // this struct field is actually an alias for the given built-in GLSL variable
    pub built_in: (Option<String>, Option<String>)
}

impl StructField {
    fn to_glsl(&self) -> Result<String, CodeGenerationError> {
        let mut result = String::new();
        result.push_str(&*try!(self.ty.format_with_name(&self.name, Space::NotArg)));
        result.push_str(";");
        Ok(result)
    }
}

#[derive(Debug)]
pub struct Function {
    pub return_ty: Type,
    pub args: Vec<Arg>,
    pub body: String
}

impl Function {
    pub fn to_glsl(&self, name: &str) -> Result<String, CodeGenerationError> {
        let mut result = String::new();
        let args: Result<Vec<_>, _> = self.args.iter().map(|arg| arg.to_glsl()).collect();
        result.push_str(&format!(
            "{ret} {name}({args}) ",
            name = name,
            ret = try!(self.return_ty.as_str(Space::NotArg)),
            args = try!(args).join(", ")
        ));
        result.push_str("{");
        result.push_str(&self.body);
        result.push_str("\n}");
        Ok(result)
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Struct(String),
    BuiltIn(&'static str),
    /// A fixed length array
    Array(Box<Type>, usize),
    /// A reference to the given type (&-ptr)
    /// It will be converted to just the plain type, as OpenGL has no pointers/references.
    Ref(Box<Type>),
    /// An mutable reference. Should be converted to GLSL `inout`
    RefMut(Box<Type>)
}

pub type FunctionName = String;

impl Type {
    pub fn as_str(&self, space: Space) -> Result<Cow<str>, CodeGenerationError> {
        match *self {
            Type::BuiltIn(name) => Ok(name.into()),
            Type::Struct(ref name) => Ok(name.deref().into()),
            Type::Array(ref ty, _) => ty.as_str(space),
            Type::Ref(ref ty) => ty.as_str(space),
            Type::RefMut(ref ty) => match space {
                Space::Arg => Ok(format!("inout {}", try!(ty.as_str(space))).into()),
                Space::NotArg => Err(CodeGenerationError::UnsupportedType("&mut reference is not supported in a variable declaration")),
                Space::Serialize => Ok(format!("inout_{}", try!(ty.as_str(space))).into())
            }
        }
    }

    /// A suffix after the name of the variable
    fn format_with_name(&self, name: &str, space: Space) -> Result<String, CodeGenerationError> {
        match *self {
            Type::Array(_, size) => Ok(format!("{ty} {name}[{size}]", ty = try!(self.as_str(space)), name = name, size = size)),
            _ => Ok(format!("{ty} {name}", ty = try!(self.as_str(space)), name = name))
        }
    }
}

pub enum Space {
    Arg,
    NotArg,
    Serialize
}

#[derive(Debug)]
pub struct Global {
    pub qualifier: Qualifier,
    pub ty: Type,
}

impl Global {
    fn to_glsl(&self, name: &str) -> Result<String, CodeGenerationError> {
        Ok(format!("{} {};", self.qualifier.to_glsl(), try!(self.ty.format_with_name(name, Space::NotArg))))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Qualifier {
    Input,
    Output,
    Uniform
}

impl Qualifier {
    fn to_glsl(&self) -> &str {
        match *self {
            Qualifier::Input => "in",
            Qualifier::Output => "out",
            Qualifier::Uniform => "uniform"
        }
    }
}

#[derive(Debug)]
pub struct Arg {
    pub ty: Type,
    pub name: String
}

impl Arg {
    pub fn new(ty: Type, name: String) -> Self {
        Arg { ty: ty, name: name }
    }

    fn to_glsl(&self) -> Result<String, CodeGenerationError> {
        self.ty.format_with_name(&self.name, Space::Arg)
    }
}