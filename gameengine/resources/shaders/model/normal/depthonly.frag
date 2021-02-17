#version 430 core

#if !defined(SUPPORT_DISPLACEMENT_OUT_OF_RANGE_DISCARDING)
#define SUPPORT_DISPLACEMENT_OUT_OF_RANGE_DISCARDING 1
#endif

in vec3 geomTangentSpacePos;
in vec2 geomUV;
in vec3 geomTangentSpaceCameraPos;

uniform sampler2D displacementTexture;

uniform float mat_displacement_multiplier;
uniform bool mat_displacement_discard_out_of_range;

//not my algorithm - https://learnopengl.com/Advanced-Lighting/Parallax-Mapping
vec2 ParallaxMapUV(const vec2 uv, const vec3 tangent_space_view_direction) //tangent_space_view_direction must be normalised
{
	const float min_layers = 8.0f;
	const float max_layers = 32.0f;
	const float num_layers = mix(min_layers, max_layers, clamp(dot(vec3(0.0f, 0.0f, 1.0f), tangent_space_view_direction), 0.0f, 1.0f));

	const float layer_depth = 1.0f / num_layers;
	const vec2 delta_uv = tangent_space_view_direction.xy * mat_displacement_multiplier * layer_depth;

	float current_depth = 0.0f;
	vec2 current_uv = uv;
	float current_depth_sample = texture(displacementTexture, current_uv).r;

	if (current_depth_sample == 0.0f)
	{
		return uv;
	}
	
	while (current_depth < current_depth_sample)
	{
		current_uv -= delta_uv;
		current_depth_sample = texture(displacementTexture, current_uv).r;
		current_depth += layer_depth;
	}

	//interpolate between last two samples (one of which was above the surface, one was below)
	vec2 prev_uv = current_uv + delta_uv;

	float prev_depth_sample_translated = texture(displacementTexture, prev_uv).r + layer_depth - current_depth;
	float current_depth_sample_translated = current_depth_sample - current_depth;

	float interpolation_weight = current_depth_sample_translated / (current_depth_sample_translated / prev_depth_sample_translated);
	return mix(prev_uv, current_uv, interpolation_weight);
}

void main()
{
	vec2 parallax_uv;
	if (mat_displacement_multiplier == 0.0f)
	{
		parallax_uv = geomUV;
	}
	else
	{	
		const vec3 tangent_space_view_dir = normalize(geomTangentSpacePos - geomTangentSpaceCameraPos);
		parallax_uv = ParallaxMapUV(geomUV, tangent_space_view_dir);

#if SUPPORT_DISPLACEMENT_OUT_OF_RANGE_DISCARDING == 1 //allows for early z testing on scenes without models that may discard even on an imperfect shader compiler
		if (mat_displacement_discard_out_of_range)
		{
			if (any(lessThan(parallax_uv, vec2(0.0f))) || any(greaterThan(parallax_uv, vec2(1.0f))))
			{
				discard;
			}
		}
#endif
	}
}