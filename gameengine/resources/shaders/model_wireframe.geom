#version 430 core
layout (triangles) in;
layout (line_strip, max_vertices = 6) out;

in vec4 vertMdlSpacePos[];
in vec4 vertSceneSpacePos[];
in vec4 vertCamSpacePos[];
in vec3 vertTangentSpacePos[];

in vec2 vertUV[];

in vec4 vertMdlSpaceNormal[];
in vec4 vertSceneSpaceNormal[];

in mat3 vertNormalTBN[];

in vec3 vertTangentSpaceCameraPos[];


uniform mat4 cam_rotate;
uniform mat4 cam_persp;


void set_outputs(int index)
{
	gl_Position = cam_persp * cam_rotate * gl_in[index % 3].gl_Position;
}

void main()
{
	gl_Layer = 0;
	for (int i = 0; i < 3; i++)
	{
		set_outputs(i);
		EmitVertex();

		set_outputs(i + 1);
		EmitVertex();

		EndPrimitive();
	}
}