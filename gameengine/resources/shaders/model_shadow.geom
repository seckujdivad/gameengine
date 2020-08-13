#version 400 core
layout (triangles) in;
layout (triangle_strip, max_vertices=18) out;

uniform mat4 cubemap_transform[6];
uniform bool is_cubemap;

out vec4 FragPos;

void main()
{
	//create six copies of the scene, one for each face of the cubemap
	for (int face_index = 0; face_index < 6; face_index++)
	{
		gl_Layer = face_index;
		for (int i = 0; i < 3; i++)
		{
			FragPos = gl_in[i].gl_Position;
			gl_Position = cubemap_transform[face_index] * FragPos;
			EmitVertex();
		}
		EndPrimitive();
	}
}