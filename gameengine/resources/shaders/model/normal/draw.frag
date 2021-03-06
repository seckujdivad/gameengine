#version 430 core

#if 0
#define APPROXIMATION_OBB_NUM 0
#define POINT_LIGHT_NUM 0
#define REFLECTION_NUM 0
#endif

//directives to avoid editor showing errors
#if !defined(POINT_LIGHT_NUM)
#define POINT_LIGHT_NUM 1
#endif

#if !defined(NUM_TEXTURES)
#define NUM_TEXTURES 3
#endif

#if !defined(NUM_NORMAL_DEPTHONLY_TEXTURES)
#define NUM_NORMAL_DEPTHONLY_TEXTURES 1
#endif

#if !defined(NUM_NORMAL_TEXTURES)
#define NUM_NORMAL_TEXTURES 1
#endif

#if !defined(APPROXIMATION_OBB_NUM)
#define APPROXIMATION_OBB_NUM 1
#endif

#if !defined(REFLECTION_NUM)
#define REFLECTION_NUM 1
#endif

#if !defined(SUPPORT_DISPLACEMENT_OUT_OF_RANGE_DISCARDING)
#define SUPPORT_DISPLACEMENT_OUT_OF_RANGE_DISCARDING 1
#endif

#if !defined(TARGET_IS_CUBEMAP)
#define TARGET_IS_CUBEMAP 1
#endif

#if TARGET_IS_CUBEMAP == 1
#define TARGET_TYPE samplerCube
#else
#define TARGET_TYPE sampler2D
#endif

//shader input-output
layout(location = 0) out vec4 colour_out[NUM_TEXTURES];

in vec3 geomMdlSpacePos;
in vec3 geomSceneSpacePos;
in vec3 geomCamSpacePos;
in vec3 geomTangentSpacePos;

in vec2 geomUV;

in vec3 geomMdlSpaceNormal;
in vec3 geomSceneSpaceNormal;
in vec3 geomCamSpaceNormal;

in mat3 geomNormalTBN;

in vec3 geomTangentSpaceCameraPos;

//model
uniform vec4 mdl_translate;
uniform mat4 mdl_rotate;
uniform mat4 mdl_scale;

//camera
uniform vec4 cam_translate;
uniform mat4 cam_rotate;
uniform mat4 cam_persp;
uniform float cam_clip_near;
uniform float cam_clip_far;
uniform mat4 cam_transform;
uniform mat4 cam_transform_inverse;

//material
uniform vec3 mat_diffuse;
uniform vec3 mat_specular;
uniform float mat_specular_highlight;
uniform float mat_displacement_multiplier;
uniform bool mat_displacement_discard_out_of_range;

// screen space reflections
uniform bool mat_ssr_enabled;
uniform float mat_ssr_resolution;
uniform float mat_ssr_max_distance;
uniform float mat_ssr_max_cast_distance;
uniform float mat_ssr_depth_acceptance;
uniform bool mat_ssr_show_this;
uniform int mat_ssr_refinements_min;
uniform int mat_ssr_refinements_max;

uniform TARGET_TYPE render_ssr_quality;

//textures
uniform sampler2D colourTexture;
uniform sampler2D normalTexture;
uniform sampler2D specularTexture;
uniform sampler2D reflectionIntensityTexture;
uniform sampler2D displacementTexture;

//lighting
uniform vec3 light_ambient;

struct PointLight
{
	vec3 position;
	vec3 intensity;
	float shadow_far_plane;
	float shadow_bias;
	int texture_dimensions;
};

#if POINT_LIGHT_NUM > 0
uniform PointLight light_points[POINT_LIGHT_NUM];
uniform samplerCubeShadow light_shadow_cubemaps[POINT_LIGHT_NUM];
#endif

uniform bool light_shadow_draw;

//reflections

uniform struct Reflection
{
	vec3 position;
	float clip_near;
	float clip_far;

	int mode; //ReflectionMode
};

uniform bool reflections_enabled;
#if REFLECTION_NUM > 0
uniform Reflection reflections[REFLECTION_NUM];
uniform samplerCube reflection_cubemaps[REFLECTION_NUM * NUM_NORMAL_TEXTURES];
uniform samplerCubeShadow reflection_depth_cubemaps[REFLECTION_NUM];
#endif
uniform int reflection_count;

//scene approximation
struct ApproximationOBB
{
	vec3 position;
	vec3 dimensions;
	mat3 rotation;
	mat3 rotation_inverse;
};

#if APPROXIMATION_OBB_NUM > 0
uniform ApproximationOBB scene_approximations[APPROXIMATION_OBB_NUM];
#endif

//skybox
uniform sampler2D skyboxMaskTexture;
uniform samplerCube skyboxTexture;

//previous render result
uniform bool render_output_valid;
uniform sampler2D render_output_colour[NUM_NORMAL_DEPTHONLY_TEXTURES];
uniform sampler2DShadow render_output_depth;
uniform ivec2 render_output_dimensions;
uniform ivec2 render_ssr_region_dimensions;

//enums
//ReflectionMode - scene/model/Reflection.h
const int ReflectionModeSimple = 0;
const int ReflectionModeOBB = 1;

vec2 SCREEN_POS = vec2(gl_FragCoord.xy / vec2(render_output_dimensions));

//functions

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

vec3 PerspDiv(const vec4 vec)
{
	return vec.xyz / vec.w;
}

//ordered dithering
float Get4x4Dither(ivec2 pos, bool correct)
{
	pos %= 4;
	const float values[16] = float[16](
		0.0f,	8.0f,	2.0f,	10.0f,
		12.0f,	4.0f,	14.0f,	6.0f,
		3.0f,	11.0f,	1.0f,	9.0f,
		15.0f,	7.0f,	13.0f,	5.0f
	);
	float value = values[pos.x + (pos.y * 4)];

	if (correct)
	{
		return (1.0f / 16.0f) * (value + 0.5f); //range: [0.5, 15.5] / 16
	}
	else
	{
		return (1.0f / 15.0f) * value; //range: [0, 1]
	}
}

float Get16x16Dither(ivec2 pos, bool correct)
{
	pos %= 16;
	const float values[64] = float[64](
		0.0f,	32.0f,	8.0f,	40.0f,	2.0f,	34.0f,	10.0f,	42.0f,
		48.0f,	16.0f,	56.0f,	24.0f,	50.0f,	18.0f,	58.0f,	26.0f,
		12.0f,	44.0f,	4.0f,	36.0f,	14.0f,	46.0f,	6.0f,	38.0f,
		60.0f,	28.0f,	52.0f,	20.0f,	62.0f,	30.0f,	54.0f,	22.0f,
		3.0f,	35.0f,	11.0f,	43.0f,	1.0f,	33.0f,	9.0f,	41.0f,
		51.0f,	19.0f,	59.0f,	27.0f,	49.0f,	17.0f,	57.0f,	25.0f,
		15.0f,	47.0f,	7.0f,	39.0f,	13.0f,	45.0f,	5.0f,	37.0f,
		63.0f,	31.0f,	55.0f,	23.0f,	61.0f,	29.0f,	53.0f,	21.0f
	);
	float value = values[pos.x + (pos.y * 16)];

	if (correct)
	{
		return (1.0f / 64.0f) * (value + 0.5f); //range: [0.5, 63.5] / 64
	}
	else
	{
		return (1.0f / 63.0f) * value; //range: [0, 1]
	}
}

#if POINT_LIGHT_NUM > 0
float GetShadowIntensity(vec3 fragpos, int lightindex)
{
	if (light_shadow_draw)
	{
		vec3 light_to_frag = fragpos - light_points[lightindex].position;
		float target_depth = length(light_to_frag) - light_points[lightindex].shadow_bias;
		float normalised_target_depth = target_depth / light_points[lightindex].shadow_far_plane;
		return texture(light_shadow_cubemaps[lightindex], vec4(light_to_frag, normalised_target_depth));
	}
	else
	{
		return 1.0f;
	}
}
#endif

/*
Finds the two points of intersection between an oriented bounding box and a line (if the points exist)
*/
struct OBBRayIntersections
{
	int count;
	vec3 positions[2];
	bool is_segment;
};

OBBRayIntersections GetFirstOBBIntersection(vec3 start_pos, vec3 direction, vec3 obb_position, vec3 obb_dimensions, mat3 obb_rotation, mat3 obb_rotation_inverse)
{
	vec3 obb_translation = obb_position - (obb_rotation * 0.5f * obb_dimensions);
	vec3 reflection_oob = obb_rotation_inverse * normalize(direction);
	vec3 fragpos_oob = obb_rotation_inverse * (start_pos - obb_translation);

	OBBRayIntersections intersections_data;
	intersections_data.count = 0;

	const int NUM_POSSIBLE_INTERSECTIONS = 6;
	vec3 intersections[NUM_POSSIBLE_INTERSECTIONS];
	
	float lambdas[NUM_POSSIBLE_INTERSECTIONS];

	const vec2 TOLERANCE = vec2(0.0001f);

	for (int i = 0; i < 3; i++)
	{
		for (int j = 0; j < 2; j++)
		{
			//at an intersection, one of the values is pinned (at a minimum or maximum) while the other two are inside the range of their maximum and minimum
			float pinned_value = (j == 0) ? 0.0f : obb_dimensions[i];
			float lambda = (pinned_value - fragpos_oob[i]) / reflection_oob[i];

			vec2 other_components = vec2(fragpos_oob[(i + 1) % 3] + (lambda * reflection_oob[(i + 1) % 3]), fragpos_oob[(i + 2) % 3] + (lambda * reflection_oob[(i + 2) % 3]));
			vec2 pin_min = vec2(0.0f) - TOLERANCE;
			vec2 pin_max = vec2(obb_dimensions[(i + 1) % 3], obb_dimensions[(i + 2) % 3]) + TOLERANCE;

			if (all(greaterThanEqual(other_components, pin_min)) && all(lessThanEqual(other_components, pin_max)))
			{
				intersections[intersections_data.count][i] = pinned_value;
				intersections[intersections_data.count][(i + 1) % 3] = other_components[0];
				intersections[intersections_data.count][(i + 2) % 3] = other_components[1];
				lambdas[intersections_data.count] = lambda;
				intersections_data.count++;
			}
		}
	}
	
	vec3 chosen_intersections[2] = vec3[2](vec3(0.0f), vec3(0.0f));
	bool chosen_intersections_have_values[2] = bool[2](false, false);
	float chosen_intersections_lambda[2] = float[2](0.0f, 0.0f);
	bool values_are_equal = false;

	for (int i = 0; i < intersections_data.count; i++)
	{
		bool lower_condition = (!chosen_intersections_have_values[0]) || (chosen_intersections_lambda[0] > lambdas[i]);
		bool upper_condition = (!chosen_intersections_have_values[1]) || (chosen_intersections_lambda[1] < lambdas[i]);

		bool trigger_both = upper_condition && lower_condition && !values_are_equal;

		if (lower_condition || trigger_both)
		{
			chosen_intersections[0] = intersections[i];
			chosen_intersections_have_values[0] = true;
			chosen_intersections_lambda[0] = lambdas[i];
		}
		
		if (upper_condition || trigger_both)
		{
			chosen_intersections[1] = intersections[i];
			chosen_intersections_have_values[1] = true;
			chosen_intersections_lambda[1] = lambdas[i];
		}

		if (trigger_both)
		{
			values_are_equal = true;
		}
		else if (upper_condition != lower_condition)
		{
			values_are_equal = false;
		}
	}

	intersections_data.positions[0] = (obb_rotation * chosen_intersections[0]) + obb_translation;
	intersections_data.positions[1] = (obb_rotation * chosen_intersections[1]) + obb_translation;

	intersections_data.is_segment = !values_are_equal;
	intersections_data.count = chosen_intersections_have_values[0] ? 2 : 0;
	
	return intersections_data;
}

vec3 GenerateErrorPattern(vec3 primary, vec3 secondary)
{
	const vec2 tiles = vec2(15);
	const vec2 tile_state = gl_FragCoord.xy / tiles;
	const int colour_index = int(tile_state.x) + int(tile_state.y);
	switch (colour_index % 2)
	{
		case 0: return primary;
		case 1: return secondary;
	};
}

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

float GetDistanceFromDepth(float depth, float clip_near, float clip_far)
{
	return (2.0f * clip_near * clip_far) / (clip_near + clip_far - (((2.0f * depth) - 1.0f) * (clip_far - clip_near)));
}

float GetDepthFromDistance(float dist, float clip_near, float clip_far)
{
	return ((clip_near + clip_far - ((2.0f * clip_near * clip_far) / dist)) / (clip_far - clip_near) + 1.0f) / 2.0f;
}

#if REFLECTION_NUM > 0
float GetDistanceFromReflection(int index, float depth_sample)
{
	return GetDistanceFromDepth(depth_sample, reflections[index].clip_near, reflections[index].clip_far);
}
#endif

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

	//get base colour
	colour_out[0] = texture(colourTexture, parallax_uv);

	//apply ambient light
	vec3 frag_intensity = vec3(0.0f);
	frag_intensity = frag_intensity + light_ambient;

	//calculate local mapped normal
	vec3 sample_normal = texture(normalTexture, parallax_uv).rgb;
	sample_normal = normalize(2.0f * (sample_normal - 0.5f));
	vec3 normal = normalize(geomNormalTBN * sample_normal);

	//sample specular map
	vec3 sample_specular = texture(specularTexture, parallax_uv).rgb * mat_specular;

	vec3 fragtocam = normalize(0 - cam_translate.xyz - geomSceneSpacePos); //get direction from the fragment to the camera

#if POINT_LIGHT_NUM > 0
	for (int i = 0; i < POINT_LIGHT_NUM; i++)
	{
		//calculate light
		vec3 light_change = vec3(0.0f);

		vec3 fragtolight = normalize(light_points[i].position - geomSceneSpacePos); //get direction from the fragment to the light source

		// diffuse
		float diffuse_intensity = max(dot(normal, fragtolight), 0.0f); //calculate diffuse intensity, floor = 0
		light_change = diffuse_intensity * mat_diffuse * light_points[i].intensity; //apply diffuse intensity and material diffuse colour to fragment intensity
		
		// specular blinn-phong
		vec3 halfway_dir = normalize(fragtolight + fragtocam);
		float specular_intensity = pow(max(dot(normal, halfway_dir), 0.0f), mat_specular_highlight);
		light_change += specular_intensity * sample_specular * light_points[i].intensity; //apply specular intensity and material specular colour to fragment intensity

		//apply light to fragment
		frag_intensity = frag_intensity + (light_change * GetShadowIntensity(geomSceneSpacePos, i));
	}
#endif

	//apply lighting to fragment
	colour_out[0] = vec4(frag_intensity, 1.0f) * colour_out[0];

	//by default, this position should resample from itself
	colour_out[1].xy = vec2(0.0f);

	//reflections
	float reflection_intensity = texture(reflectionIntensityTexture, parallax_uv).r;
	bool ssr_reflection_applied = false;
	vec3 reflection_colour = vec3(0.0f, 0.0f, 0.0f);
	{
		if (render_output_valid && mat_ssr_enabled && length(geomCamSpacePos) < mat_ssr_max_distance)
		{
			colour_out[1].xy = vec2(-1.0f);
			const vec3 direction = normalize(reflect(-fragtocam, normal)); //direction to trace the reflections in
			const vec3 start_pos = geomSceneSpacePos; //position to start the trace from

			//find end pos - this is where the ray leaves the screen
			//use an adapted oriented bounding box algorithm in screen space to find this point
			vec3 end_pos;
			{
				const vec4 start = vec4(start_pos, 1.0f);
				const vec4 translate = vec4(direction, 0.0f);

				const vec4 transformed_start = cam_transform * start;
				const vec4 transformed_direction = cam_transform * translate;

				float lambdas[2] = float[2](-1.0f, -1.0f);
				int intersection_index = 0;

				for (int i = 0; i < 3; i++)
				{
					for (int j = 0; j < 2; j++)
					{
						float pinned_value = (j == 0) ? -1.0f : 1.0f;

						float lambda = ((pinned_value * transformed_start.w) - transformed_start[i]) / (transformed_direction[i] - (pinned_value * transformed_direction.w));

						vec3 position = PerspDiv(transformed_start + (lambda * transformed_direction));

						vec2 other_components = vec2(position[(i + 1) % 3], position[(i + 2) % 3]);
						if (all(greaterThan(other_components, vec2(-1.0f))) && all(lessThan(other_components, vec2(1.0f))))
						{
							lambdas[intersection_index] = lambda;
							intersection_index++;
						}
					}
				}

				float lambda = max(lambdas[0], lambdas[1]);
				lambda = min(lambda, mat_ssr_max_cast_distance);
				end_pos = start.xyz + (lambda * direction.xyz);
			}

			//apply perspective transformation to start and end points
			const vec3 ss_start_pos = PerspDiv(cam_transform * vec4(start_pos, 1.0f));
			const vec3 ss_end_pos = PerspDiv(cam_transform * vec4(end_pos, 1.0f));

			//get previous frame ssr hit percentage and apply some transformations to get better results
			float ssr_hit_perc = SampleTarget(render_ssr_quality, SCREEN_POS).r;
			if (ssr_hit_perc > 0.0f)
			{
				ssr_hit_perc = 1.0f - ((1.0f - ssr_hit_perc) * 0.25f);
			}

			/*
			Test pixels are pixels that are selected to run at full quality
			This means that regions that are too low quality to show any
			reflections, but should be reflecting an object, are (often,
			eventually) found.
			*/
			bool is_test_px = ivec2(gl_FragCoord.xy) % 32 == ivec2(0);
			float mixer = is_test_px ? 1.0f : ssr_hit_perc;
			int initial_search_level = int(mix(float(mat_ssr_refinements_max), float(mat_ssr_refinements_min), mixer)); //number of levels of precision that can be dropped through before any hits become final

			int search_level = initial_search_level;

			const float num_searches_on_refine = 2.0f; //number of searches to 
			float depth_acceptance = mat_ssr_depth_acceptance * pow(num_searches_on_refine, initial_search_level);
			float half_acceptance = 0.5f * depth_acceptance;

			const vec3 ss_direction = ss_end_pos - ss_start_pos; //screen space trace direction
			
			/*
			Calculate the hit increment so that each step moves by initial_pixel_stride in one axis
			and by less in the other (XY screen space). If initial_pixel_stride == 1, this essentially
			becomes floating point Bresenham.
			*/
			float hit_increment;
			{
				const float initial_pixel_stride = mat_ssr_resolution * pow(num_searches_on_refine, initial_search_level);

				const bool x_is_most_significant_direction = abs(ss_direction.x) > abs(ss_direction.y);
				const vec2 divisors = render_output_dimensions * ss_direction.xy;

				hit_increment = (2.0f * initial_pixel_stride) / abs(divisors[int(!x_is_most_significant_direction)]);
			}

			/*
			Traces a ray in the screen space, checking at regular intervals if that pixel's depth is in range
			If it is, this counts as a hit. On a hit, the size of the intervals and the acceptance range are both
			decreased. Also, the ray position moves back one interval. If another hit is found before twice the
			original interval is traversed, the search is refined again. If not, the search goes back to being less
			precise. Once the search level reaches 0, any hits found become the final result.
			*/

			vec3 ss_position = ss_start_pos; //current ray positon in screen space
			float hit_pos = 0.0f; //fraction of the length of the ray covered
			int increments_at_this_level = 0; //tracks the number of increments that have taken place since the search level was last changed

			vec2 tex_pos = vec2(0.0f); //current ray position in screen space (the vector needed for sampling from the screen textures)
			while (!ssr_reflection_applied && hit_pos < 1.0f) //repeat until either the ray reaches the edge of the screen or a reflection hit is found
			{
				//convert screen space position to use texture UV space coordinates
				float depth_max = GetDepthFromDistance(GetDistanceFromDepth(ss_position.z, cam_clip_near, cam_clip_far) + half_acceptance, cam_clip_near, cam_clip_far);
				float depth_min = GetDepthFromDistance(GetDistanceFromDepth(ss_position.z, cam_clip_near, cam_clip_far) - half_acceptance, cam_clip_near, cam_clip_far);

				const float EPSILON = 0.000001f;
				depth_max = (depth_max * 0.5f) + 0.5f + EPSILON;
				depth_min = (depth_min * 0.5f) + 0.5f - EPSILON;

				tex_pos = (ss_position.xy * 0.5f) + 0.5f;

				float max_hits = texture(render_output_depth, vec3(tex_pos.xy, depth_max)); //number of samples that are less than or equal to the maximum value
				float min_hits = texture(render_output_depth, vec3(tex_pos.xy, depth_min)); //number of samples that are less than or equal to the minimum value
				float samples_in_range = abs(max_hits - min_hits);

				if ((samples_in_range > 0.5f) && (texture(render_output_colour[0], tex_pos.xy).r > 0.5f)) //a hit was found
				{
					//if the search increment is as small as is allowed then use this hit as the final location
					ssr_reflection_applied = search_level == 0;

					//make the search finer
					hit_pos -= hit_increment;

					hit_increment /= num_searches_on_refine;

					depth_acceptance /= num_searches_on_refine;
					half_acceptance = depth_acceptance * 0.5f;

					search_level -= 1;

					increments_at_this_level = 0;
				}

				increments_at_this_level++;

				/*
				No hits have been found at this level in 2x the length of the next largest interval
				length - increase the level by 1 as the last hit was likely a near miss instead of
				a true hit.
				*/
				if (search_level != initial_search_level && increments_at_this_level - 1 > 2 * int(num_searches_on_refine))
				{
					hit_increment *= num_searches_on_refine;
					depth_acceptance *= num_searches_on_refine;
					search_level += 1;
					increments_at_this_level = 0;
				}

				//find screen space position of next test
				hit_pos += hit_increment;

				//linearly interpolate position in screen space - see source list
				ss_position.xy = mix(ss_start_pos.xy, ss_end_pos.xy, hit_pos);
				ss_position.z = 1.0f / mix(1.0f / ss_start_pos.z, 1.0f / ss_end_pos.z, hit_pos);
			}

			if (ssr_reflection_applied)
			{
				colour_out[1].xy = tex_pos.xy; //set the position on screen to resample from
				reflection_colour = vec3(0.0f);
			}
		}

		if (!ssr_reflection_applied)
		{
#if REFLECTION_NUM > 0
			if (reflections_enabled)
			{
				//find reflection to use
				float refl_distance = length(reflections[0].position - geomSceneSpacePos);
				int reflection_index = 0;
				for (int i = 1; i < reflection_count; i++)
				{
					float current_distance = length(reflections[i].position - geomSceneSpacePos);

					bool is_closer = current_distance < refl_distance;
					reflection_index = is_closer ? i : reflection_index;
					refl_distance = mix(refl_distance, current_distance, is_closer);
				}
			
				if (reflections[reflection_index].mode == ReflectionModeSimple) //no perspective correction
				{
					vec3 sample_vector = reflect(-fragtocam, normal);

					bool is_skybox = texture(reflection_depth_cubemaps[reflection_index], vec4(sample_vector, 1.0f)).r == 1.0f;
					reflection_colour = is_skybox ? texture(skyboxTexture, sample_vector).rgb : texture(reflection_cubemaps[reflection_index * NUM_NORMAL_TEXTURES], sample_vector).rgb;
				}
				else if (reflections[reflection_index].mode == ReflectionModeOBB) //oriented bounding box
				{
					vec3 all_intersections[2 * APPROXIMATION_OBB_NUM]; //2d array with start and end positions of all line segments
					int search_index = -1; //index of a line segment that contains the point of reflection
					vec3 refl_dir = normalize(reflect(-fragtocam, normal)); //direction to cast the rays in
					bool valid_segments[APPROXIMATION_OBB_NUM]; //whether or not each pair in all_intersections is a valid line segment or just junk data

					for (int i = 0; i < APPROXIMATION_OBB_NUM; i++)
					{
						OBBRayIntersections intersection_data = GetFirstOBBIntersection(geomSceneSpacePos, refl_dir, scene_approximations[i].position, scene_approximations[i].dimensions, scene_approximations[i].rotation, scene_approximations[i].rotation_inverse); //find where (if anywhere) the reflection passes through this specific OBB

						//write OBB intersection info into proper storage locations
						all_intersections[i * 2] = intersection_data.positions[0];
						all_intersections[(i * 2) + 1] = intersection_data.positions[1];

						valid_segments[i] = intersection_data.count == 2;

						//check if the line segment is both valid and overlaps the reflection point (i.e. starts on one side and ends on the other)
						const float tolerance = 0.01f;
						const vec3 tolerance_vec = tolerance * refl_dir;
						const bool intersection_0_before_point = dot(intersection_data.positions[0] - geomSceneSpacePos - tolerance_vec, refl_dir) < 0.0f;
						const bool intersection_1_after_point = dot(intersection_data.positions[1] - geomSceneSpacePos + tolerance_vec, refl_dir) > 0.0f;

						search_index = valid_segments[i] && intersection_0_before_point && intersection_1_after_point ? i : search_index;
					}
				
					if (search_index == -1)
					{
						reflection_colour = GenerateErrorPattern(vec3(1.0f, 0.0f, 0.0f), vec3(1.0f)); //error state - no line segment overlapped with the reflection point
					}
					else
					{
						bool included_segments[APPROXIMATION_OBB_NUM];
						for (int i = 0; i < APPROXIMATION_OBB_NUM; i++)
						{
							included_segments[i] = i == search_index; //initialise array - at the start, only the search_index segment has been used to construct the line
						}

						const vec3 line_start = all_intersections[search_index * 2]; //no need to extend the start point backwards, should be prevented by the refl surface and couldn't affect the result anyway
						vec3 line_end = all_intersections[(search_index * 2) + 1];
						int line_end_index = search_index;

						{
							const float tolerance = 0.01f;
							const vec3 tolerance_vec = tolerance * refl_dir;

							for (int i = 0; i < APPROXIMATION_OBB_NUM; i++)
							{
								for (int j = 0; j < APPROXIMATION_OBB_NUM; j++)
								{
									const bool segment_can_be_included = valid_segments[j] && !included_segments[j]; //don't check line segments that are already included in the line

									//check if extending end is possible
									// (of the existing line segment)
									const bool starts_after_start = dot(all_intersections[j * 2] - line_start + tolerance_vec, refl_dir) > 0.0f;
									const bool starts_before_end = dot(all_intersections[j * 2] - line_end - tolerance_vec, refl_dir) < 0.0f;
									const bool ends_after_end = dot(all_intersections[(j * 2) + 1] - line_end - tolerance_vec, refl_dir) > 0.0f;

									const bool extend_line = segment_can_be_included && starts_after_start && starts_before_end && ends_after_end; //new line segment overlaps with end of current line segment - extend line
									included_segments[j] = extend_line ? true : included_segments[j];
									line_end = extend_line ? all_intersections[(j * 2) + 1] : line_end;
									line_end_index = extend_line ? j : line_end_index;
								}
							}
						}

						//find nearest reflection to end of line
						int reflection_index = 0;
						{
							float current_length = length(reflections[0].position - line_end);
							for (int i = 1; i < reflection_count; i++)
							{
								float new_length = length(reflections[i].position - line_end);
								bool condition = new_length < current_length;
								
								reflection_index = condition ? i : reflection_index;
								current_length = condition ? new_length : current_length;
							}
						}

						//sample using the final values
						{
							//we want to access texture reflection_index, but all texture array accesses must be dynamically uniform (i.e. the same in all invocations) so we access all of them but immediately discard all but reflection_index
							vec3 reflection_sample = vec3(0.0f);
							bool sample_is_skybox = false;
							for (int i = 0; i < reflection_count; i++)
							{
								bool valid_sample = i == reflection_index;
								vec3 sample_vector = line_end - reflections[i].position;
								reflection_sample += float(valid_sample) * texture(reflection_cubemaps[i * NUM_NORMAL_TEXTURES], sample_vector).rgb;
								sample_is_skybox = sample_is_skybox || (valid_sample && (texture(reflection_depth_cubemaps[i], vec4(sample_vector, 1.0f)) == 1.0f));
							}

							reflection_colour = sample_is_skybox ? texture(skyboxTexture,  refl_dir).rgb : reflection_sample;
						}
					}
				}
				else
				{
					reflection_colour = GenerateErrorPattern(vec3(1.0f, 0.0f, 0.0f), vec3(0.0f, 1.0f, 0.0f)); //unknown reflection mode
				}
			}
			else
#endif
			{
				reflection_intensity = 0.0f;
				reflection_colour = vec3(0.0f);
			}
		}
	}

	//apply skybox
	vec3 skybox_intensity = texture(skyboxMaskTexture, geomUV).rgb;
	colour_out[0] *= vec4(1.0f - skybox_intensity, 1.0f);
	colour_out[0] += vec4(skybox_intensity * texture(skyboxTexture, geomSceneSpacePos + cam_translate.xyz).rgb, 0.0f);

	//this shader can't produce translucent fragments
	colour_out[0].a = 1.0f;

	//pass on the reflection intensity
	colour_out[2].rgb = reflection_colour;
	colour_out[2].a = reflection_intensity;
}