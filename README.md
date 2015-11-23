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

This will spit out the following shader to `shader_vertex.glsl`:

    #version 330
    struct Vertex {
         vec3 v_position;
         vec3 v_color;
    };
    struct UniformBlock {
         vec3 v_shift;
    };
    struct VertexOutput {
         vec4 v_position;
         vec3 v_color;
    };
    in vec3 vertex_in_Vertex_0_v_position;
    in vec3 vertex_in_Vertex_0_v_color;
    out vec4 fragment_in_VertexOutput_0_v_position;
    out vec3 fragment_in_VertexOutput_0_v_color;
    layout(std140) uniform u_UniformBlock_0 {
         UniformBlock v_u_UniformBlock_0;
    };
    VertexOutput shader_vertex(Vertex v_v, UniformBlock v_u) {
        return VertexOutput(
            vec4((v_v.v_position.x + v_u.v_shift.x), (v_v.v_position.y + v_u.v_shift.y), (v_v.v_position.z + v_u.v_shift.z), 1.0),
            v_v.v_color
        );
    }
    void main() {
        VertexOutput result = shader_vertex(Vertex(vertex_in_Vertex_0_v_position, vertex_in_Vertex_0_v_color), v_u_UniformBlock_0);
        gl_Position = result.v_position;
        fragment_in_VertexOutput_0_v_position = result.v_position;
        fragment_in_VertexOutput_0_v_color = result.v_color;
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