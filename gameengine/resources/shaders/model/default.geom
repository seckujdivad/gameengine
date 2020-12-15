#version 430 core
layout (triangles) in;
layout (triangle_strip, max_vertices = 18) out;

in vec4 teseMdlSpacePos[];
in vec4 teseSceneSpacePos[];
in vec4 teseCamSpacePos[];
in vec3 teseTangentSpacePos[];

in vec2 teseUV[];

in vec4 teseMdlSpaceNormal[];
in vec4 teseSceneSpaceNormal[];

in mat3 teseNormalTBN[];

in vec3 teseTangentSpaceCameraPos[];

out vec4 geomMdlSpacePos;
out vec4 geomSceneSpacePos;
out vec4 geomCamSpacePos;
out vec3 geomTangentSpacePos;

out vec2 geomUV;

out vec4 geomMdlSpaceNormal;
out vec4 geomSceneSpaceNormal;
out vec4 geomCamSpaceNormal;

out mat3 geomNormalTBN;

out vec3 geomTangentSpaceCameraPos;

uniform mat4 cubemap_transform[6];
uniform bool is_cubemap;

uniform mat4 cam_rotate;
uniform mat4 cam_persp;

void set_outputs(int index)
{
	geomMdlSpacePos = teseMdlSpacePos[index % 3];
	geomSceneSpacePos = teseSceneSpacePos[index % 3];
	geomCamSpacePos = teseCamSpacePos[index % 3];
	geomTangentSpacePos = teseTangentSpacePos[index & 3];

	geomUV = teseUV[index % 3];

	geomMdlSpaceNormal = teseMdlSpaceNormal[index % 3];
	geomSceneSpaceNormal = teseSceneSpaceNormal[index % 3];

	geomNormalTBN = teseNormalTBN[index % 3];

	geomTangentSpaceCameraPos = teseTangentSpaceCameraPos[index % 3];

	//calculate post camera transformation values (which are affected by the cubemap face)
	geomCamSpacePos = (cubemap_transform[gl_Layer] * cam_rotate * geomCamSpacePos) / geomCamSpacePos.w;

	gl_Position = cam_persp * geomCamSpacePos;

	geomCamSpaceNormal = cubemap_transform[gl_Layer] * cam_rotate * teseSceneSpaceNormal[index % 3];
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