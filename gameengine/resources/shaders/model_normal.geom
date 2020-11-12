#version 430 core
layout (triangles) in;
layout (triangle_strip, max_vertices = 18) out;

in vec4 vertMdlSpacePos[];
in vec4 vertSceneSpacePos[];
in vec4 vertCamSpacePos[];
in vec3 vertTangentSpacePos[];

in vec2 vertUV[];

in vec4 vertMdlSpaceNormal[];
in vec4 vertSceneSpaceNormal[];

in mat3 vertNormalTBN[];

in vec3 vertTangentSpaceCameraPos[];

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
	geomMdlSpacePos = vertMdlSpacePos[index % 3];
	geomSceneSpacePos = vertSceneSpacePos[index % 3];
	geomCamSpacePos = vertCamSpacePos[index % 3];
	geomTangentSpacePos = vertTangentSpacePos[index & 3];

	geomUV = vertUV[index % 3];

	geomMdlSpaceNormal = vertMdlSpaceNormal[index % 3];
	geomSceneSpaceNormal = vertSceneSpaceNormal[index % 3];

	geomNormalTBN = vertNormalTBN[index % 3];

	geomTangentSpaceCameraPos = vertTangentSpaceCameraPos[index % 3];

	//calculate post camera transformation values (which are affected by the cubemap face)
	geomCamSpacePos = (cubemap_transform[gl_Layer] * cam_rotate * geomCamSpacePos) / geomCamSpacePos.w;

	gl_Position = cam_persp * geomCamSpacePos;

	geomCamSpaceNormal = cubemap_transform[gl_Layer] * cam_rotate * vertSceneSpaceNormal[index % 3];
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