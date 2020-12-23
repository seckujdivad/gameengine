#version 400 core
layout(vertices = 16) out;

in vec4 vertMdlSpacePos[];
in vec4 vertSceneSpacePos[];
in vec4 vertCamSpacePos[];
in vec3 vertTangentSpacePos[];

in vec2 vertUV[];

in vec4 vertMdlSpaceNormal[];
in vec4 vertSceneSpaceNormal[];

out vec4 tescMdlSpacePos[];
out vec4 tescSceneSpacePos[];
out vec4 tescCamSpacePos[];

out vec2 tescUV[];

out vec4 tescMdlSpaceNormal[];
out vec4 tescSceneSpaceNormal[];

uniform bool tess_enable;

void main()
{
	tescMdlSpacePos[gl_InvocationID] = vertMdlSpacePos[gl_InvocationID];
	tescSceneSpacePos[gl_InvocationID] = vertSceneSpacePos[gl_InvocationID];
	tescCamSpacePos[gl_InvocationID] = vertCamSpacePos[gl_InvocationID];

	tescUV[gl_InvocationID] = vertUV[gl_InvocationID];

	tescMdlSpaceNormal[gl_InvocationID] = vertMdlSpaceNormal[gl_InvocationID];
	tescSceneSpaceNormal[gl_InvocationID] = vertSceneSpaceNormal[gl_InvocationID];
	
	if (tess_enable)
	{
		gl_TessLevelOuter[0] = 5.0f;
		gl_TessLevelOuter[1] = 5.0f;
		gl_TessLevelOuter[2] = 5.0f;
		gl_TessLevelOuter[3] = 5.0f;

		gl_TessLevelInner[0] = 5.0f;
		gl_TessLevelInner[1] = 5.0f;
	}
	else
	{
		gl_TessLevelOuter[0] = 1.0f;
		gl_TessLevelOuter[1] = 1.0f;
		gl_TessLevelOuter[2] = 1.0f;
		gl_TessLevelOuter[3] = 1.0f;

		gl_TessLevelInner[0] = 1.0f;
		gl_TessLevelInner[1] = 1.0f;
	}
}