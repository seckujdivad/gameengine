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
#define TARGET_TYPE_SHADOW samplerCubeShadow
#else
#define TARGET_TYPE sampler2D
#define TARGET_TYPE_SHADOW sampler2DShadow
#endif

layout(location = 0) out vec4 colour_out[NUM_TEXTURES];

in vec2 geomUV;

uniform TARGET_TYPE draw_frame[NUM_NORMAL_DRAW_TEXTURES];
uniform TARGET_TYPE_SHADOW draw_frame_depth;

uniform ivec2 render_output_dimensions;

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

float SampleTarget(TARGET_TYPE_SHADOW to_sample, vec2 coords, float comparison)
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

	return texture(to_sample, vec4(cubemap_coords_transformed, comparison));
#else
	return texture(to_sample, vec3(coords, comparison));
#endif
}

void main()
{
	const vec2 QUARTER_PIXEL_THRESHOLD = vec2(0.25f / render_output_dimensions);
	
	const vec4 skybox_colour = vec4(vec3(0.0f), 0.0f); //transparent background

	//get and write out depth
	float depth_samples = SampleTarget(draw_frame_depth, geomUV, 1.0f);
	bool resample_is_skybox = depth_samples > 0.5f;

	//get ssr sample
	vec2 ssr_sample = SampleTarget(draw_frame[1], geomUV).xy;
	bool ssr_hit_found = any(greaterThan(ssr_sample, QUARTER_PIXEL_THRESHOLD));

	//apply ssr sample if it exists
	vec4 draw_frame_2_sample = SampleTarget(draw_frame[2], geomUV);
	vec3 reflection_colour = ssr_hit_found ? SampleTarget(draw_frame[0], ssr_sample).rgb : draw_frame_2_sample.rgb;
	float reflection_intensity = draw_frame_2_sample.a;

	//calculate final colour
	colour_out[0].rgba = resample_is_skybox ? skybox_colour : SampleTarget(draw_frame[0], geomUV).rgba + vec4(reflection_colour * reflection_intensity, 0.0f);
}