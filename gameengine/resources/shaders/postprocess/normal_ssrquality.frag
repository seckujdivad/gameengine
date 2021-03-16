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

uniform ivec2 render_draw_output_dimensions;
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

void main()
{
	//calculate % ssr hits
	// sample each pixel in region
	//   if uv vector == 0.0f then skip
	//   else if uv vector == -1.0f then increment attempt count
	//   else increment attempt count and increment hit count

	vec2 region_radius_scr = 0.5f / (vec2(render_draw_output_dimensions) / vec2(render_output_dimensions));
	ivec2 region_start = ivec2((geomUV - region_radius_scr) * render_draw_output_dimensions);
	ivec2 region_end = ivec2((geomUV + region_radius_scr) * render_draw_output_dimensions);

	const vec2 QUARTER_PIXEL_DRAW_DIMENSIONS = 0.25f / vec2(render_draw_output_dimensions);

	int ssr_attempt_count = 0;
	int ssr_hit_count = 0;

	for (int x = region_start.x; x < region_end.x; x++)
	{
		for (int y = region_start.y; y < region_end.y; y++)
		{
			ivec2 pixel_vec = ivec2(x, y);
			vec2 sample_vec = pixel_vec / render_draw_output_dimensions;
			vec2 ssr_uv_sample = SampleTarget(draw_frame[1], sample_vec).xy;

			if (all(lessThan(ssr_uv_sample, 0.0f - QUARTER_PIXEL_DRAW_DIMENSIONS)))
			{
				ssr_attempt_count++;
			}
			else if (all(greaterThan(ssr_uv_sample, QUARTER_PIXEL_DRAW_DIMENSIONS)))
			{
				ssr_attempt_count++;
				ssr_hit_count++;
			}
		}
	}

	colour_out[0].r = ssr_attempt_count == 0 ? 0.0f
		: 1.0f - pow(1.0f - (float(ssr_hit_count) / float(ssr_attempt_count)), 2);
}