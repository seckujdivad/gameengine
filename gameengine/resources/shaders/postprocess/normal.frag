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
	const vec2 QUARTER_PIXEL_THRESHOLD = vec2(0.25f / render_output_dimensions);
	
	const vec4 skybox_colour = vec4(vec3(0.0f), 0.0f); //transparent background

	//get and write out depth
	float depth = SampleTarget(draw_frame_depth, geomUV).r;
	gl_FragDepth = depth;
	bool resample_is_skybox = depth == 1.0f;

	//get ssr sample
	vec2 ssr_sample = SampleTarget(draw_frame[1], geomUV).xy;
	bool ssr_hit_found = true;
	if (all(lessThan(ssr_sample, QUARTER_PIXEL_THRESHOLD)))
	{
		//do box sample of region
		const vec2 float_dimensions = vec2(render_output_dimensions);
		const vec2 pixel_pos = geomUV * float_dimensions;

		//3x3 box sample
		const int BOX_SAMPLE_SIZE = 7;
		const int BOX_SAMPLE_RADIUS = BOX_SAMPLE_SIZE / 2;

		float best_sample_len = BOX_SAMPLE_SIZE * BOX_SAMPLE_SIZE;
		vec2 best_sample = vec2(0.0f);

		float angles[BOX_SAMPLE_SIZE * BOX_SAMPLE_SIZE];
		int angles_len = 0;

		for (int x = 0 - BOX_SAMPLE_RADIUS; x < BOX_SAMPLE_RADIUS + 1; x++)
		{
			for (int y = 0 - BOX_SAMPLE_RADIUS; y < BOX_SAMPLE_RADIUS + 1; y++)
			{
				vec2 pixel_offset = vec2(x, y);

				vec2 sample_pos = (pixel_pos + pixel_offset) / float_dimensions;
				vec2 uv_sample = SampleTarget(draw_frame[1], sample_pos).xy;

				float sample_len = length(pixel_offset);

				if ((sample_len < best_sample_len) && all(greaterThan(uv_sample, QUARTER_PIXEL_THRESHOLD))) //translate ssr sample
				{
					best_sample = uv_sample;
					best_sample_len = sample_len;

					float angle_cosine = dot(vec2(1.0f, 0.0f), pixel_offset) / sample_len;
					float angle = acos(angle_cosine);
					angle = y >= 0 ? angle : mod((0.0f - angle), radians(360));

					angles[angles_len] = angle;
					angles_len++;
				}
			}
		}

		if (angles_len == 0)
		{
			ssr_hit_found = false;
		}
		else if (angles_len == 1)
		{
			if (BOX_SAMPLE_SIZE > 1)
			{
				ssr_hit_found = false;
			}
			else
			{
				ssr_hit_found = true;
				ssr_sample = best_sample;
			}
		}
		else
		{
			bool region_is_large_enough = true;
			if (angles_len < (BOX_SAMPLE_SIZE * BOX_SAMPLE_SIZE) / 2) //less than half of the region is made up of samples
			{
				float max_neighbour_angle = 0.0f;
				for (int i = 0; i < angles_len; i++)
				{
					float neighbour_angle = radians(360);
					for (int j = 0; j < angles_len; j++)
					{
						if (i != j)
						{
							float angle_diff = abs(angles[i] - angles[j]);
							if (angle_diff < neighbour_angle)
							{
								neighbour_angle = angle_diff;
							}
						}
					}

					if (neighbour_angle > max_neighbour_angle)
					{
						max_neighbour_angle = neighbour_angle;
					}
				}

				region_is_large_enough = max_neighbour_angle > radians(180);
			}

			if (region_is_large_enough)
			{
				ssr_hit_found = true;
				ssr_sample = best_sample;
			}
			else
			{
				ssr_hit_found = false;
			}
		}
	}

	//apply ssr sample if it exists
	vec4 draw_frame_2_sample = SampleTarget(draw_frame[2], geomUV);
	vec3 reflection_colour = ssr_hit_found ? SampleTarget(draw_frame[0], ssr_sample).rgb : draw_frame_2_sample.rgb;
	float reflection_intensity = draw_frame_2_sample.a;

	//calculate final colour
	colour_out[0].rgba = resample_is_skybox ? skybox_colour : SampleTarget(draw_frame[0], geomUV).rgba + vec4(reflection_colour * reflection_intensity, 0.0f);
}