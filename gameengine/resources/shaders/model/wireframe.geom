#version 400 core
layout (triangles) in;
layout (line_strip, max_vertices = 6) out;

in vec3 teseMdlSpacePos[];
in vec3 teseSceneSpacePos[];
in vec3 teseCamSpacePos[];

in vec2 teseUV[];

in vec3 teseMdlSpaceNormal[];
in vec3 teseSceneSpaceNormal[];


uniform mat4 cam_rotate;
uniform mat4 cam_persp;

uniform bool draw_back_faces;


void main()
{
	bool draw = true;
	if (!draw_back_faces)
	{
		vec3 normal = normalize(teseSceneSpaceNormal[0] + teseSceneSpaceNormal[1] + teseSceneSpaceNormal[2]);
		draw = dot(normal, 0.0f - teseCamSpacePos[0]) > 0.0f;
	}

	if (draw)
	{
		vec4 positions[3];
		for (int i = 0; i < 3; i++)
		{
			positions[i] = cam_persp * cam_rotate * vec4(teseCamSpacePos[i], 1.0f);
		}

		for (int i = 0; i < 3; i++)
		{
			gl_Position = positions[i];
			EmitVertex();

			gl_Position = positions[(i + 1) % 3];
			EmitVertex();

			EndPrimitive();
		}
	}
}