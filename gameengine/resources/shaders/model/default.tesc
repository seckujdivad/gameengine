#version 400 core
layout(vertices = 16) out;

in vec3 vertMdlSpacePos[];
in vec3 vertSceneSpacePos[];
in vec3 vertCamSpacePos[];

in vec2 vertUV[];

in vec3 vertMdlSpaceNormal[];
in vec3 vertSceneSpaceNormal[];

out vec3 tescMdlSpacePos[];
out vec3 tescSceneSpacePos[];
out vec3 tescCamSpacePos[];

out vec2 tescUV[];

out vec3 tescMdlSpaceNormal[];
out vec3 tescSceneSpaceNormal[];

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
		const float outer_level = 5.0f;
		const float inner_level = 5.0f;

		gl_TessLevelOuter[0] = outer_level;
		gl_TessLevelOuter[1] = outer_level;
		gl_TessLevelOuter[2] = outer_level;
		gl_TessLevelOuter[3] = outer_level;
		
		gl_TessLevelInner[0] = inner_level;
		gl_TessLevelInner[1] = inner_level;
	}
	else
	{
		const float outer_level = 1.0f;
		const float inner_level = 1.0f;

		gl_TessLevelOuter[0] = outer_level;
		gl_TessLevelOuter[1] = outer_level;
		gl_TessLevelOuter[2] = outer_level;
		gl_TessLevelOuter[3] = outer_level;
		
		gl_TessLevelInner[0] = inner_level;
		gl_TessLevelInner[1] = inner_level;
	}
}