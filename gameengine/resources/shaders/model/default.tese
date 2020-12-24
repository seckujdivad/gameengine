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

float Power(const float a, const int b) //pow(0, 0) is undefined behaviour according to the spec, but is a condition hit by these algorithms - give the contextually correct value of 1
{
	if ((a == 0.0f) && (b == 0))
	{
		return 1.0f;
	}
	else
	{
		return pow(a, float(b));
	}
}

float BezierBasisMult(const int i, const int n, const float t)
{
	return float(BinomialCoefficient(n, i)) * Power(t, i) * Power(1.0f - t, n - i);
}

vec3 interpolate(const vec3 values[gl_MaxPatchVertices], const vec3 position)
{
	vec3 sum = vec3(0.0f);
	for (int i = 0; i < patch_size_u; i++)
	{
		vec3 inner_sum = vec3(0.0f);
		for (int j = 0; j < patch_size_v; j++)
		{
			inner_sum += BezierBasisMult(j, patch_size_v - 1, position.y) * values[(i * patch_size_v) + j];
		}

		sum += inner_sum * BezierBasisMult(i, patch_size_u - 1, position.x);
	}

	return sum;
}

vec2 interpolate(const vec2 values[gl_MaxPatchVertices], const vec3 position)
{
	vec3 values_vec3[gl_MaxPatchVertices];
	for (int i = 0; i < gl_MaxPatchVertices; i++)
	{
		values_vec3[i] = vec3(values[i], 0.0f);
	}
	return interpolate(values_vec3, position).xy;
}

float BezierDerivativeMult(const int i, const int n, float t)
{
	t = clamp(t, 0.001f, 0.999f); //this function divides by zero at t=0 and t=1, workaround is just to ensure it never quite reaches those values
	return float(BinomialCoefficient(n, i)) * Power(t, i) * Power(1.0f - t, n - i) * ((float(i) / t) + (float(i - n) / (1.0f - t)));
}

vec3 derivative_u(const vec3 values[gl_MaxPatchVertices], const vec3 position)
{
	vec3 sum = vec3(0.0f);
	for (int i = 0; i < patch_size_u; i++)
	{
		vec3 inner_sum = vec3(0.0f);
		for (int j = 0; j < patch_size_v; j++)
		{
			inner_sum += BezierBasisMult(j, patch_size_v - 1, position.y) * values[(i * patch_size_v) + j];
		}
		sum += inner_sum * BezierDerivativeMult(i, patch_size_u - 1, position.x);
	}

	return sum;
}

vec3 derivative_v(const vec3 values[gl_MaxPatchVertices], const vec3 position)
{
	vec3 sum = vec3(0.0f);
	for (int j = 0; j < patch_size_v; j++)
	{
		vec3 inner_sum = vec3(0.0f);
		for (int i = 0; i < patch_size_u; i++)
		{
			inner_sum += BezierBasisMult(i, patch_size_u - 1, position.x) * values[(i * patch_size_v) + j];
		}
		sum += inner_sum * BezierDerivativeMult(j, patch_size_v - 1, position.y);
	}

	return sum;
}

void main()
{
	teseMdlSpacePos = interpolate(tescMdlSpacePos, gl_TessCoord);
	teseSceneSpacePos = interpolate(tescSceneSpacePos, gl_TessCoord);
	teseCamSpacePos = interpolate(tescCamSpacePos, gl_TessCoord);

	teseUV = interpolate(tescUV, gl_TessCoord);

	if (tess_enable)
	{
		vec3 tangent = derivative_u(tescMdlSpacePos, gl_TessCoord);
		vec3 bitangent = derivative_v(tescMdlSpacePos, gl_TessCoord);

		teseMdlSpaceNormal = 0.0f - normalize(cross(tangent, bitangent));
		teseSceneSpaceNormal = persp_div(mdl_rotate * vec4(teseMdlSpaceNormal, 1.0f));
	}
	else
	{
		teseMdlSpaceNormal = interpolate(tescMdlSpaceNormal, gl_TessCoord);
		teseSceneSpaceNormal = interpolate(tescSceneSpaceNormal, gl_TessCoord);
	}
}