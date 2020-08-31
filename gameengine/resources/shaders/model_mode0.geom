#version 430 core
layout (triangles) in;
layout (triangle_strip, max_vertices = 18) out;

in vec4 vertMdlSpacePos[];
in vec4 vertSceneSpacePos[];
in vec4 vertCamSpacePos[];

in vec2 vertUV[];

in vec4 vertMdlSpaceNormal[];
in vec4 vertSceneSpaceNormal[];
in vec4 vertCamSpaceNormal[];

in mat3 vertNormalTBN[];

out vec4 geomMdlSpacePos;
out vec4 geomSceneSpacePos;
out vec4 geomCamSpacePos;

out vec2 geomUV;

out vec4 geomMdlSpaceNormal;
out vec4 geomSceneSpaceNormal;
out vec4 geomCamSpaceNormal;

out mat3 geomNormalTBN;

uniform mat4 cubemap_transform[6];
uniform bool is_cubemap;

void set_outputs(int index)
{
	geomMdlSpacePos = vertMdlSpacePos[index % 3];
	geomSceneSpacePos = vertSceneSpacePos[index % 3];
	geomCamSpacePos = vertCamSpacePos[index % 3];

	geomUV = vertUV[index % 3];

	geomMdlSpaceNormal = vertMdlSpaceNormal[index % 3];
	geomSceneSpaceNormal = vertSceneSpaceNormal[index % 3];
	geomCamSpaceNormal = vertCamSpaceNormal[index % 3];

	geomNormalTBN = vertNormalTBN[index % 3];

	gl_Position = gl_in[index % 3].gl_Position;
}

void main()
{
	int num_layers = is_cubemap ? 6 : 1;

	for (int layer = 0; layer < num_layers; layer++)
	{
		for (int i = 0; i < 3; i++)
		{
			gl_Layer = layer;
			set_outputs(i);
			EmitVertex();
		}
		EndPrimitive();
	}
}