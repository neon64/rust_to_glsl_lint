#![feature(plugin)]
#![plugin(rust_to_glsl_lint)]

extern crate nalgebra as na;
use na::{Vec2, Vec3, Vec4};

use std::fs::File;
use std::io::Read;

struct Vertex {
    position: Vec3<f32>,
    color: Vec3<f32>
}

struct UniformBlock {
    shift: Vec3<f32>,
}

struct VertexOutput {
    #[glsl_repr(output="vertex-position")]
    position: Vec4<f32>,
    color: Vec3<f32>
}

#[shader(vertex)]
fn shader_vertex(v: Vertex, u: &UniformBlock) -> VertexOutput {
    VertexOutput {
        position: Vec4::new(v.position.x + u.shift.x, v.position.y + u.shift.y, v.position.z + u.shift.z, 1.0),
        color: v.color
    }
}

#[test]
fn test_compile() {
    let mut file = File::open("tests/expected.glsl").unwrap();
    let mut expected = String::new();
    file.read_to_string(&mut expected).unwrap();
    let mut file2 = File::open("tests/shader_vertex.glsl").unwrap();
    let mut result = String::new();
    file2.read_to_string(&mut result).unwrap();
    assert_eq!(expected, result);
}