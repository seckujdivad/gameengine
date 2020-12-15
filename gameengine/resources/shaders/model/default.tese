#version 400 core
layout(triangles, equal_spacing, ccw) in;

in vec4 tescMdlSpacePos[];
in vec4 tescSceneSpacePos[];
in vec4 tescCamSpacePos[];
in vec3 tescTangentSpacePos[];

in vec2 tescUV[];

in vec4 tescMdlSpaceNormal[];
in vec4 tescSceneSpaceNormal[];

in mat3 tescNormalTBN[];

in vec3 tescTangentSpaceCameraPos[];

out vec4 teseMdlSpacePos;
out vec4 teseSceneSpacePos;
out vec4 teseCamSpacePos;
out vec3 teseTangentSpacePos;

out vec2 teseUV;

out vec4 teseMdlSpaceNormal;
out vec4 teseSceneSpaceNormal;

out mat3 teseNormalTBN;

out vec3 teseTangentSpaceCameraPos;

vec4 interpolate(vec4 values[gl_MaxPatchVertices])
{
	return vec4(gl_TessCoord.x) * values[0] + vec4(gl_TessCoord.y) * values[1] + vec4(gl_TessCoord.z) * values[2];
}

vec3 interpolate(vec3 values[gl_MaxPatchVertices])
{
	return vec3(gl_TessCoord.x) * values[0] + vec3(gl_TessCoord.y) * values[1] + vec3(gl_TessCoord.z) * values[2];
}

vec2 interpolate(vec2 values[gl_MaxPatchVertices])
{
	return vec2(gl_TessCoord.x) * values[0] + vec2(gl_TessCoord.y) * values[1] + vec2(gl_TessCoord.z) * values[2];
}

void main()
{
	teseMdlSpacePos = interpolate(tescMdlSpacePos);
	teseSceneSpacePos = interpolate(tescSceneSpacePos);
	teseCamSpacePos = interpolate(tescCamSpacePos);
	teseTangentSpacePos = interpolate(tescTangentSpacePos);

	teseUV = interpolate(tescUV);

	teseMdlSpaceNormal = interpolate(tescMdlSpaceNormal);
	teseSceneSpaceNormal = interpolate(tescSceneSpaceNormal);

	teseNormalTBN = tescNormalTBN[0];

	teseTangentSpaceCameraPos = interpolate(tescTangentSpaceCameraPos);
}