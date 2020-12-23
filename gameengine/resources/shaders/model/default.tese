#version 400 core
layout(quads, equal_spacing, ccw) in;

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

uniform bool tess_enable;

const int patch_size = 4;
const int patch_degree = patch_size - 1;

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
	if (!((t - 1.0f == 0.0f) && (n - i == 0)))
	{
		end_mult = pow(1.0f - t, n - i);
	}

	return float(BinomialCoefficient(n, i)) * start_mult * end_mult;
}

vec4 interpolate(const vec4 values[gl_MaxPatchVertices])
{
	vec4 sum = vec4(0.0f);
	for (int i = 0; i < patch_size; i++)
	{
		float basis_x = BezierBasisMult(i, patch_degree, gl_TessCoord.x);
		vec4 inner_sum = vec4(0.0f);
		for (int j = 0; j < patch_size; j++)
		{
			inner_sum += BezierBasisMult(j, patch_degree, gl_TessCoord.y) * values[(i * patch_size) + j];
		}

		sum += inner_sum * basis_x;
	}

	return sum;
}

vec3 interpolate(const vec3 values[gl_MaxPatchVertices])
{
	vec4 values_vec4[gl_MaxPatchVertices];
	for (int i = 0; i < gl_MaxPatchVertices; i++)
	{
		values_vec4[i] = vec4(values[i], 0.0f);
	}
	return interpolate(values_vec4).xyz;
}

vec2 interpolate(const vec2 values[gl_MaxPatchVertices])
{
	vec4 values_vec4[gl_MaxPatchVertices];
	for (int i = 0; i < gl_MaxPatchVertices; i++)
	{
		values_vec4[i] = vec4(values[i], 0.0f, 0.0f);
	}
	return interpolate(values_vec4).xy;
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