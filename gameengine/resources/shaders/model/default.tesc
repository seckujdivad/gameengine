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


uniform int patch_size_u;
uniform int patch_size_v;

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
		vec3 corner_directions[4];
		corner_directions[0] = normalize(vertCamSpacePos[0]);
		corner_directions[1] = normalize(vertCamSpacePos[patch_size_v - 1]);
		corner_directions[2] = normalize(vertCamSpacePos[(patch_size_u - 1) * patch_size_v]);
		corner_directions[3] = normalize(vertCamSpacePos[((patch_size_u - 1) * patch_size_v) + patch_size_u - 1]);

		float val = dot(corner_directions[0], corner_directions[1]);

		float edge_angle_cosines[4];
		edge_angle_cosines[0] = dot(corner_directions[0], corner_directions[1]); //top
		edge_angle_cosines[1] = dot(corner_directions[0], corner_directions[2]); //right
		edge_angle_cosines[2] = dot(corner_directions[3], corner_directions[2]); //bottom
		edge_angle_cosines[3] = dot(corner_directions[3], corner_directions[1]);; //left

		for (int i = 0; i < 4; i++)
		{
			gl_TessLevelOuter[i] = max(30.0f * abs(sin(acos(edge_angle_cosines[i]))), 1.0f);
		}
		
		gl_TessLevelInner[0] = (gl_TessLevelOuter[1] + gl_TessLevelOuter[3]) / 2;
		gl_TessLevelInner[1] = (gl_TessLevelOuter[0] + gl_TessLevelOuter[2]) / 2;
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