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

uniform bool draw_back_faces;


void main()
{
	vec3 positions[3];
	for (int i = 0; i < 3; i++)
	{
		vec4 vertex = cam_rotate * gl_in[i].gl_Position;
		positions[i] = vertex.xyz / vertex.w;
	}
	
	bool draw_tri = true;
	if (!draw_back_faces)
	{
		//ccw winding is default
		vec3 first = positions[1] - positions[0];
		vec3 second = positions[2] - positions[0];
		vec3 normal = cross(first, second);

		draw_tri = dot(normal, positions[0]) < 0.0f;
	}

	if (draw_tri)
	{
		for (int i = 0; i < 3; i++)
		{
			gl_Position = cam_persp * vec4(positions[i], 1.0f);
			EmitVertex();

			gl_Position = cam_persp * vec4(positions[(i + 1) % 3], 1.0f);
			EmitVertex();

			EndPrimitive();
		}
	}
}