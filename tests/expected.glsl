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
