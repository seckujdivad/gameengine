#version 430 core
layout (triangles) in;
layout (triangle_strip, max_vertices = 3) out;

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

void set_outputs(int index, int face_index, bool is_cubemap)
{
	geomMdlSpacePos = vertMdlSpacePos[index % 3];
	geomSceneSpacePos = vertSceneSpacePos[index % 3];
	geomCamSpacePos = vertCamSpacePos[index % 3];

	if (is_cubemap)
	{
		geomMdlSpacePos = cubemap_transform[face_index] * geomMdlSpacePos;
		geomSceneSpacePos = cubemap_transform[face_index] * geomSceneSpacePos;
		geomCamSpacePos = cubemap_transform[face_index] * geomCamSpacePos;
	}

	geomUV = vertUV[index % 3];

	geomMdlSpaceNormal = vertMdlSpaceNormal[index % 3];
	geomSceneSpaceNormal = vertSceneSpaceNormal[index % 3];
	geomCamSpaceNormal = vertCamSpaceNormal[index % 3];

	if (is_cubemap)
	{
		geomMdlSpaceNormal = cubemap_transform[face_index] * geomMdlSpaceNormal;
		geomSceneSpaceNormal = cubemap_transform[face_index] * geomSceneSpaceNormal;
		geomCamSpaceNormal = cubemap_transform[face_index] * geomCamSpaceNormal;
	}

	geomNormalTBN = vertNormalTBN[index % 3];

	if (is_cubemap)
	{
		geomNormalTBN[0] = mat3(cubemap_transform[face_index]) * geomNormalTBN[0];
		geomNormalTBN[1] = mat3(cubemap_transform[face_index]) * geomNormalTBN[1];
		geomNormalTBN[2] = mat3(cubemap_transform[face_index]) * geomNormalTBN[2];
	}

	gl_Position = gl_in[index % 3].gl_Position;

	if (is_cubemap)
	{
		gl_Position = cubemap_transform[face_index] * gl_Position;
	}
}

void main()
{
	for (int face_index = 0; face_index < (is_cubemap ? 6 : 1); face_index++)
	{
		gl_Layer = face_index;
		for (int i = 0; i < 3; i++)
		{
			set_outputs(i, face_index, is_cubemap);
			EmitVertex();
		}
		EndPrimitive();
	}
}