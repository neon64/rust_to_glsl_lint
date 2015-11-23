# Rust to GLSL Converter

This converts Rust code into a GLSL shader, through the magic of a custom lint pass. Therefore it requires a nightly build of Rust.

Here is a basic usage example:

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

## Intrinsics & Native Interoperability

The `#[glsl_repr]` attribute can be used to specify how a struct field is mapped to a GLSL built in.

Currently there is only one supported built-in:

    #[glsl_repr(output="vertex-position")]

Instead of using global variables, function inputs become `in` globals, outputs become `out` globals, and inputs that are taken by reference (`&T`) are `uniform` variables.

## Limitations

- Can't translate function bodies of external crates (because the lint pass doesn't have the AST)
- Could be fooled into creating invalid GLSL
- Doesn't support textures