#version 400 core
layout(vertices = 16) out;

in vec3 vertMdlSpacePos[];
in vec3 vertCamSpacePos[];
in vec2 vertUV[];
in vec3 vertMdlSpaceNormal[];

out vec3 tescMdlSpacePos[];
out vec2 tescUV[];
out vec3 tescMdlSpaceNormal[];


uniform int patch_size_u;
uniform int patch_size_v;

uniform bool tess_enable;


float CalcMinCosine(const int array_start, const int array_step, const int num_items)
{
	vec3 values[4];
	for (int i = 0; i < num_items; i++)
	{
		values[i] = normalize(vertCamSpacePos[array_start + (array_step * i)]);
	}

	float cosines[12];
	int array_index = 0;
	for (int i = 0; i < num_items; i++)
	{
		for (int j = 0; j < num_items; j++)
		{
			if (i != j)
			{
				cosines[array_index] = dot(values[i], values[j]);
				array_index++;
			}
		}
	}

	float min_cosine = 1.0f;
	for (int i = 0; i < num_items * (num_items - 1); i++)
	{
		if (abs(min_cosine) > abs(cosines[i]))
		{
			min_cosine = cosines[i];
		}
	}
	return min_cosine;
}

void main()
{
	tescMdlSpacePos[gl_InvocationID] = vertMdlSpacePos[gl_InvocationID];
	tescUV[gl_InvocationID] = vertUV[gl_InvocationID];
	tescMdlSpaceNormal[gl_InvocationID] = vertMdlSpaceNormal[gl_InvocationID];
	
	if (tess_enable)
	{
		int corners[4]; //indices of the four corners of the patch
		corners[0] = 0;
		corners[1] = patch_size_v - 1;
		corners[2] = (patch_size_u - 1) * patch_size_v;
		corners[3] = ((patch_size_u - 1) * patch_size_v) + patch_size_u - 1;

		float edge_angle_cosines[4];
		edge_angle_cosines[0] = CalcMinCosine(corners[0], 1, patch_size_v); //0, 1 - top
		edge_angle_cosines[1] = CalcMinCosine(corners[0], patch_size_v, patch_size_u); //0, 2 - right
		edge_angle_cosines[2] = CalcMinCosine(corners[2], 1, patch_size_v); //3, 2 - bottom
		edge_angle_cosines[3] = CalcMinCosine(corners[1], patch_size_v, patch_size_u); //3, 1 - left

		for (int i = 0; i < 4; i++)
		{
			gl_TessLevelOuter[i] = 1.0f + max(20.0f * sin(acos(edge_angle_cosines[i])), 0.0f);
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