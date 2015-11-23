#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ShaderType {
    Vertex,
    Fragment
}

impl ShaderType {
    pub fn from_str(message: &str) -> Option<Self> {
        match message {
            "vertex" => Some(ShaderType::Vertex),
            "fragment" => Some(ShaderType::Fragment),
            _ => None
        }
    }
}