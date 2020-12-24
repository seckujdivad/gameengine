#version 400 core
layout(quads, equal_spacing, ccw) in;

in vec3 tescMdlSpacePos[];
in vec3 tescSceneSpacePos[];
in vec3 tescCamSpacePos[];

in vec2 tescUV[];

in vec3 tescMdlSpaceNormal[];
in vec3 tescSceneSpaceNormal[];

out vec3 teseMdlSpacePos;
out vec3 teseSceneSpacePos;
out vec3 teseCamSpacePos;

out vec2 teseUV;

out vec3 teseMdlSpaceNormal;
out vec3 teseSceneSpaceNormal;


uniform mat4 mdl_rotate;

uniform bool tess_enable;

uniform int patch_size_u;
uniform int patch_size_v;


vec3 persp_div(vec4 vec)
{
	return vec.xyz / vec.w;
}

int Factorial(const int n)
{
	int product = 1;
	for (int i = n; i > 1; i--)
	{
		product *= i;
	}

	return max(product, 1);
}

int BinomialCoefficient(const int n, const int k)
{
	return Factorial(n) / (Factorial(k) * Factorial(n - k));
}

float BezierBasisMult(const int i, const int n, const float t)
{
	float start_mult = 1.0f; //pow(0, 0) is undefined behaviour according to the spec, but is a condition hit by this algorithm - give the contextually correct value of 1
	if (!((t == 0.0f) && (i == 0)))
	{
		start_mult = pow(t, i);
	}

	float end_mult = 1.0f;
	if (!((1.0f - t == 0.0f) && (n - i == 0)))
	{
		end_mult = pow(1.0f - t, n - i);
	}

	return float(BinomialCoefficient(n, i)) * start_mult * end_mult;
}

vec4 interpolate(const vec4 values[gl_MaxPatchVertices], const vec3 position)
{
	vec4 sum = vec4(0.0f);
	for (int i = 0; i < patch_size_u; i++)
	{
		vec4 inner_sum = vec4(0.0f);
		for (int j = 0; j < patch_size_v; j++)
		{
			inner_sum += BezierBasisMult(j, patch_size_v - 1, position.y) * values[(i * patch_size_u) + j];
		}

		sum += inner_sum * BezierBasisMult(i, patch_size_u - 1, position.x);
	}

	return sum;
}

vec3 interpolate(const vec3 values[gl_MaxPatchVertices], const vec3 position)
{
	vec4 values_vec4[gl_MaxPatchVertices];
	for (int i = 0; i < gl_MaxPatchVertices; i++)
	{
		values_vec4[i] = vec4(values[i], 0.0f);
	}
	return interpolate(values_vec4, position).xyz;
}

vec2 interpolate(const vec2 values[gl_MaxPatchVertices], const vec3 position)
{
	vec4 values_vec4[gl_MaxPatchVertices];
	for (int i = 0; i < gl_MaxPatchVertices; i++)
	{
		values_vec4[i] = vec4(values[i], 0.0f, 0.0f);
	}
	return interpolate(values_vec4, position).xy;
}

void main()
{
	teseMdlSpacePos = interpolate(tescMdlSpacePos, gl_TessCoord);
	teseSceneSpacePos = interpolate(tescSceneSpacePos, gl_TessCoord);
	teseCamSpacePos = interpolate(tescCamSpacePos, gl_TessCoord);

	teseUV = interpolate(tescUV, gl_TessCoord);

	if (tess_enable)
	{
		//approximate the normals using the adjacent tangents and bitangents
		vec3 u_increment = vec3(1.0f / gl_TessLevelInner[0], 0.0f, 0.0f);
		vec3 v_increment = vec3(0.0f, 1.0f / gl_TessLevelInner[1], 0.0f);

		vec3 tangent = interpolate(tescMdlSpacePos, min(gl_TessCoord + u_increment, vec3(1.0f))) - interpolate(tescMdlSpacePos, max(gl_TessCoord - u_increment, vec3(0.0f)));
		vec3 bitangent = interpolate(tescMdlSpacePos, min(gl_TessCoord + v_increment, vec3(1.0f))) - interpolate(tescMdlSpacePos, max(gl_TessCoord - v_increment, vec3(0.0f)));

		teseMdlSpaceNormal = 0.0f - normalize(cross(tangent, bitangent));
		teseSceneSpaceNormal = persp_div(mdl_rotate * vec4(teseMdlSpaceNormal, 1.0f));
	}
	else
	{
		teseMdlSpaceNormal = interpolate(tescMdlSpaceNormal, gl_TessCoord);
		teseSceneSpaceNormal = interpolate(tescSceneSpaceNormal, gl_TessCoord);
	}
}