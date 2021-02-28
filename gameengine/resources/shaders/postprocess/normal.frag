#version 430 core

#if !defined(NUM_TEXTURES)
#define NUM_TEXTURES 1
#endif

#if !defined(NUM_NORMAL_DRAW_TEXTURES)
#define NUM_NORMAL_DRAW_TEXTURES 3
#endif

#if !defined(TARGET_IS_CUBEMAP)
#define TARGET_IS_CUBEMAP 1
#endif

#if TARGET_IS_CUBEMAP == 1
#define TARGET_TYPE samplerCube
#else
#define TARGET_TYPE sampler2D
#endif

layout(location = 0) out vec4 colour_out[NUM_TEXTURES];

in vec2 geomUV;

uniform TARGET_TYPE draw_frame[NUM_NORMAL_DRAW_TEXTURES];
uniform TARGET_TYPE draw_frame_depth;

vec4 SampleTarget(TARGET_TYPE to_sample, vec2 coords)
{
#if TARGET_IS_CUBEMAP == 1
	vec3 cubemap_coords = vec3((coords - 0.5f) * 2.0f, 1.0f);

	const vec3 axis_mults[6] = vec3[6](
		vec3(-1.0f, -1.0f, 1.0f), //x+: flip x and y (face space), +x face
		vec3(1.0f, -1.0f, -1.0f), //x-
		vec3(1.0f, 1.0f, 1.0f), //y+
		vec3(1.0f, -1.0f, -1.0f), //y-
		vec3(1.0f, -1.0f, 1.0f), //z+
		vec3(-1.0f, -1.0f, -1.0f) //z-
	);

	const ivec3 axis_remaps[6] = ivec3[6](
		ivec3(2, 1, 0), //x+: res x = in z, res y = in y, res z = in x
		ivec3(2, 1, 0), //x-
		ivec3(0, 2, 1), //y+
		ivec3(0, 2, 1), //y-
		ivec3(0, 1, 2), //z+
		ivec3(0, 1, 2) //z-
	);

	//mult is applied first, then remap
	vec3 cubemap_coords_transformed = vec3(0.0f);
	for (int i = 0; i < 3; i++)
	{
		int index = axis_remaps[gl_Layer][i];
		cubemap_coords_transformed[i] = cubemap_coords[index] * axis_mults[gl_Layer][index];
	}

	return texture(to_sample, cubemap_coords_transformed);
#else
	return texture(to_sample, coords);
#endif
}

void main()
{
	const vec4 skybox_colour = vec4(vec3(0.0f), 0.0f); //transparent background

	//get and write out depth
	float depth = SampleTarget(draw_frame_depth, geomUV).r;
	gl_FragDepth = depth;
	bool resample_is_skybox = depth == 1.0f;

	//get resample UV location
	vec2 ssr_sample_uv = SampleTarget(draw_frame[1], geomUV).xy;

	//sample the reflection intensity
	vec4 refl_intensity;
	bool apply_refl_intensity;
	{
		vec4 tex_sample = SampleTarget(draw_frame[2], geomUV);
		refl_intensity = vec4(tex_sample.rgb, 0.0f);
		apply_refl_intensity = tex_sample.a > 0.5f;
	}

	vec4 colour_sample =  SampleTarget(draw_frame[0], geomUV);
	vec4 colour_resample =  SampleTarget(draw_frame[0], ssr_sample_uv);
	vec4 final_sample = apply_refl_intensity ? colour_sample + (colour_resample * refl_intensity) : colour_resample;

	colour_out[0] = resample_is_skybox ? skybox_colour : final_sample;
}