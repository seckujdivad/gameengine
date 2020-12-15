#version 400 core
layout(vertices = 3) out;

in vec4 vertMdlSpacePos[];
in vec4 vertSceneSpacePos[];
in vec4 vertCamSpacePos[];
in vec3 vertTangentSpacePos[];

in vec2 vertUV[];

in vec4 vertMdlSpaceNormal[];
in vec4 vertSceneSpaceNormal[];

in mat3 vertNormalTBN[];

in vec3 vertTangentSpaceCameraPos[];

out vec4 tescMdlSpacePos[];
out vec4 tescSceneSpacePos[];
out vec4 tescCamSpacePos[];
out vec3 tescTangentSpacePos[];

out vec2 tescUV[];

out vec4 tescMdlSpaceNormal[];
out vec4 tescSceneSpaceNormal[];

out mat3 tescNormalTBN[];

out vec3 tescTangentSpaceCameraPos[];

uniform bool tess_enable;

void main()
{
	tescMdlSpacePos[gl_InvocationID] = vertMdlSpacePos[gl_InvocationID];
	tescSceneSpacePos[gl_InvocationID] = vertSceneSpacePos[gl_InvocationID];
	tescCamSpacePos[gl_InvocationID] = vertCamSpacePos[gl_InvocationID];
	tescTangentSpacePos[gl_InvocationID] = vertTangentSpacePos[gl_InvocationID];

	tescUV[gl_InvocationID] = vertUV[gl_InvocationID];

	tescMdlSpaceNormal[gl_InvocationID] = vertMdlSpaceNormal[gl_InvocationID];
	tescSceneSpaceNormal[gl_InvocationID] = vertSceneSpaceNormal[gl_InvocationID];

	tescNormalTBN[gl_InvocationID] = vertNormalTBN[gl_InvocationID];

	tescTangentSpaceCameraPos[gl_InvocationID] = vertTangentSpaceCameraPos[gl_InvocationID];

	if (tess_enable)
	{
		gl_TessLevelOuter[0] = 2.0f;
		gl_TessLevelOuter[1] = 4.0f;
		gl_TessLevelOuter[2] = 6.0f;
		gl_TessLevelOuter[3] = 8.0f;

		gl_TessLevelInner[0] = 8.0f;
		gl_TessLevelInner[1] = 8.0f;
	}
	else
	{
		for (int i = 0; i < 4; i++)
		{
			gl_TessLevelOuter[i] = 1.0f;
		}

		for (int i = 0; i < 2; i++)
		{
			gl_TessLevelInner[i] = 1.0f;
		}
	}
}