use structure::ShaderType;
use rustc::session::Session;
use syntax::codemap::Span;
use glsl_optimizer::{Context, Target, Options, ShaderType as OShaderType};
use glsl_optimizer::error::OptimizationError;
use term;

pub fn optimize_shader(sess: &Session, span: Span, ty: ShaderType, source: String) -> Result<String, ()> {
    let mut context = Context::new(Target::OpenGL);
    let ty = match ty {
        ShaderType::Vertex => OShaderType::Vertex,
        ShaderType::Fragment => OShaderType::Fragment
    };
    let mut shader = context.optimize(ty, &source, Options::empty());
    shader.get_output().map_err(|string| {
        let errors = OptimizationError::from_string(&string);
        sess.span_err(
            span,
            "failed to optimize the GLSL shader converted from the following Rust function:"
        );
        println!("\n");
        let mut t = term::stdout().unwrap();
        for error in errors {
            print!("{line}:{column} ", line = error.line, column = error.column);
            t.fg(term::color::RED).unwrap();
            print!("{ty}: ", ty = error.ty);
            t.reset().unwrap();
            print!("{message}\n", message = error.message);
        }
        println!("\nGLSL Shader Source:\n");
        write_numbered_lines(&source);
        println!("\n");
    })
}

/// Writes out source code, with each line numbered.
pub fn write_numbered_lines(source: &str) {
    let mut line_number = 1;
    for line in source.lines() {
        println!("{}:\t{}", line_number, line);
        line_number = line_number + 1;
    }
}