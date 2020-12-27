#version 430 core

//directives to avoid editor showing errors
#if !defined(POINT_LIGHT_NUM)
#define POINT_LIGHT_NUM 1
#endif

#if !defined(DATA_TEX_NUM)
#define DATA_TEX_NUM 1
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

//shader input-output
layout(location = 0) out vec4 frag_out;
layout(location = 1) out vec4 data_out[DATA_TEX_NUM];

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
uniform int mat_ssr_refinements;

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
};

uniform PointLight light_points[POINT_LIGHT_NUM];
uniform samplerCube light_shadow_cubemaps[POINT_LIGHT_NUM];

uniform bool light_shadow_draw;

//reflections

uniform struct Reflection
{
	vec3 position;
	float clip_near;
	float clip_far;

	int mode; //ReflectionMode

	int iterations;
};

uniform bool reflections_enabled;
uniform Reflection reflections[REFLECTION_NUM];
uniform samplerCube reflection_cubemaps[REFLECTION_NUM];
uniform samplerCube reflection_depth_cubemaps[REFLECTION_NUM];
uniform samplerCube reflection_data_cubemaps[REFLECTION_NUM * DATA_TEX_NUM];
uniform int reflection_count;

//scene approximation
struct ApproximationOBB
{
	vec3 position;
	vec3 dimensions;
	mat3 rotation;
	mat3 rotation_inverse;
};

uniform ApproximationOBB scene_approximations[APPROXIMATION_OBB_NUM]; 

//skybox
uniform sampler2D skyboxMaskTexture;
uniform samplerCube skyboxTexture;

//previous render result
uniform bool render_output_valid;
uniform sampler2D render_output_colour;
uniform sampler2D render_output_depth;
uniform sampler2D render_output_data[DATA_TEX_NUM];
uniform int render_output_x;
uniform int render_output_y;

//enums
//ReflectionMode - scene/model/Reflection.h
const int ReflectionModeIterative = 0;
const int ReflectionModeOBB = 1;

//functions

float GetShadowIntensity(vec3 fragpos, int lightindex)
{
	if (light_shadow_draw)
	{
		vec3 lighttofrag = fragpos - light_points[lightindex].position;

		float depth_sample = texture(light_shadow_cubemaps[lightindex], lighttofrag).r;
		float corrected_depth_sample = depth_sample * light_points[lightindex].shadow_far_plane;
		float frag_depth = length(lighttofrag);

		bool frag_in_range = depth_sample != 1.0f;
		bool frag_obscured = (frag_depth - light_points[lightindex].shadow_bias) > corrected_depth_sample;
		bool frag_in_shadow = frag_in_range && frag_obscured;

		return float(!frag_in_shadow);
	}
	else
	{
		return 1.0f;
	}
}

/*
Finds the two points of intersection between an oriented bounding box and a line (if the points exist)
*/
void GetFirstOBBIntersection(vec3 start_pos, vec3 direction, vec3 obb_position, vec3 obb_dimensions, mat3 obb_rotation, mat3 obb_rotation_inverse, out bool isvalid, out vec3 results[2])
{
	vec3 obb_translation = obb_position - (obb_rotation * 0.5f * obb_dimensions);
	vec3 reflection_oob = obb_rotation_inverse * normalize(direction);
	vec3 fragpos_oob = obb_rotation_inverse * (start_pos - obb_translation);

	vec3 intersections[2];
	int intersection_index = 0;
	float lambdas[2];

	isvalid = false;

	for (int i = 0; i < 3; i++)
	{
		for (int j = 0; j < 2; j++)
		{
			//at an intersection, one of the values is pinned (at a minimum or maximum) while the other two are inside the range of their maximum and minimum
			float pinned_value = (j == 0) ? 0.0f : obb_dimensions[i];
			float lambda = (pinned_value - fragpos_oob[i]) / reflection_oob[i];
			//intersection_index = (lambda > 0.0) ? 1 : 0;

			float second_axis_value = fragpos_oob[(i + 1) % 3] + (lambda * reflection_oob[(i + 1) % 3]);
			float third_axis_value = fragpos_oob[(i + 2) % 3] + (lambda * reflection_oob[(i + 2) % 3]);
			if ((0.0f <= second_axis_value) && (second_axis_value <= obb_dimensions[(i + 1) % 3]) &&
				(0.0f <= third_axis_value) && (third_axis_value <= obb_dimensions[(i + 2) % 3]))
			{
				intersections[intersection_index][i] = pinned_value;
				intersections[intersection_index][(i + 1) % 3] = second_axis_value;
				intersections[intersection_index][(i + 2) % 3] = third_axis_value;
				lambdas[intersection_index] = lambda;
				isvalid = true;
				intersection_index++;
			}
		}
	}

	results[0] = (obb_rotation * intersections[0]) + obb_translation;
	results[1] = (obb_rotation * intersections[1]) + obb_translation;

	if (lambdas[0] > lambdas[1])
	{
		vec3 swap = results[0];
		results[0] = results[1];
		results[1] = swap;
	}
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
	frag_out = texture(colourTexture, parallax_uv);

	//apply ambient light
	vec3 frag_intensity = vec3(0.0f);
	frag_intensity = frag_intensity + light_ambient;

	//calculate local mapped normal
	vec3 sample_normal = texture(normalTexture, parallax_uv).rgb;
	sample_normal = normalize(2.0f * (sample_normal - 0.5f));
	vec3 normal = normalize(geomNormalTBN * sample_normal);

	//sample specular map
	vec3 sample_specular = texture(specularTexture, parallax_uv).rgb * mat_specular;

	vec3 fragtocam = normalize(0 - vec3(cam_translate) - geomSceneSpacePos); //get direction from the fragment to the camera

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

	//apply lighting to fragment
	frag_out = vec4(frag_intensity, 1.0f) * frag_out;

	//reflections
	vec3 reflection_intensity = texture(reflectionIntensityTexture, parallax_uv).rgb;
	vec3 reflection_colour = vec3(0.0f, 0.0f, 0.0f);
	{
		bool ssr_reflection_applied = false;
		if (render_output_valid && mat_ssr_enabled)
		{
			if (length(geomCamSpacePos) < mat_ssr_max_distance)
			{
				const vec3 direction = normalize(reflect(-fragtocam, normal));
				const vec3 end_pos = geomSceneSpacePos + (direction * mat_ssr_max_cast_distance);
				const vec3 start_pos = geomSceneSpacePos + (direction * ((mat_ssr_depth_acceptance * 1.01f) / dot(direction, normal)));

				const vec4 ss_start_pos_prediv = cam_transform * vec4(start_pos, 1.0f);
				const vec3 ss_start_pos = ss_start_pos_prediv.xyz / ss_start_pos_prediv.w;

				const vec4 ss_end_pos_prediv = cam_transform * vec4(end_pos, 1.0f);
				const vec3 ss_end_pos = ss_end_pos_prediv.xyz / ss_end_pos_prediv.w;

				const vec3 ss_direction = ss_end_pos - ss_start_pos;

				int search_level = mat_ssr_refinements; //number of additional searches to carry out

				const float num_searches_on_refine = 2.0f;
				float depth_acceptance = mat_ssr_depth_acceptance * pow(num_searches_on_refine, mat_ssr_refinements);
				
				float hit_increment;
				{
					const float initial_pixel_stride = mat_ssr_resolution * pow(num_searches_on_refine, mat_ssr_refinements);

					const bool x_is_most_significant_direction = abs(ss_direction.x) > abs(ss_direction.y);
					const float divisor = (int(x_is_most_significant_direction) * render_output_x * ss_direction.x)
						+ (int(!x_is_most_significant_direction) + render_output_y * ss_direction.y);

					hit_increment = (2.0f * initial_pixel_stride) / abs(divisor);
				}

				vec3 ss_position = ss_start_pos;
				float hit_pos = 0.0f;

				while (!ssr_reflection_applied && all(greaterThan(ss_position, vec3(-1.0f))) && all(lessThan(ss_position, vec3(1.0f))) && (hit_pos < 1.0f))
				{
					//convert screen space position to use texture UV space coordinates
					const vec2 tex_pos = (ss_position.xy * 0.5f) + 0.5f;
					const float sample_depth = (texture(render_output_depth, tex_pos.xy).r * 2.0f) - 1.0f;

					const float sample_depth_camspace = 2.0 * cam_clip_near * cam_clip_far / (cam_clip_far + cam_clip_near - sample_depth * (cam_clip_far - cam_clip_near));
					const float search_depth_camspace = 2.0 * cam_clip_near * cam_clip_far / (cam_clip_far + cam_clip_near - ss_position.z * (cam_clip_far - cam_clip_near));

					const bool hit_detected = (-1.0f < sample_depth) && (sample_depth < 1.0f)
						&& (texture(render_output_data[0], tex_pos.xy).r > 0.5f)
						&& (abs(sample_depth_camspace - search_depth_camspace) < depth_acceptance);

					if (hit_detected && (search_level == 0)) //a hit was found and the search increment is as small as is allowed, use this hit as the final location
					{
						reflection_colour = texture(render_output_colour, tex_pos.xy).rgb;
						ssr_reflection_applied = true;
					}
					else if (hit_detected) //the search can still be made finer
					{
						hit_pos -= hit_increment;
						hit_increment /= num_searches_on_refine;
						depth_acceptance /= num_searches_on_refine;
						search_level -= 1;
					}

					//find screen space position of next test
					hit_pos = hit_pos + hit_increment;

					ss_position.xy = mix(ss_start_pos.xy, ss_end_pos.xy, hit_pos);
					ss_position.z = 1 / mix(1 / ss_start_pos.z, 1 / ss_end_pos.z, hit_pos);
				}
			}
		}

		if (!ssr_reflection_applied)
		{
			if (reflections_enabled)
			{
				//find reflection to use
				float refl_distance = 0.0f;
				int reflection_index;
				for (int i = 0; i < reflection_count; i++)
				{
					float current_distance = length(reflections[i].position - geomSceneSpacePos);

					bool condition = (refl_distance == 0.0f) || (current_distance < refl_distance);
					reflection_index = condition ? i : reflection_index;
					refl_distance = mix(int(condition), refl_distance, current_distance);
				}
			
				if (reflections[reflection_index].mode == ReflectionModeIterative) //iteratively apply perspective correction
				{
					vec3 sample_vector = reflect(-fragtocam, normal);
					float sample_space_length = reflections[reflection_index].clip_far - reflections[reflection_index].clip_near;
					vec3 offset = geomSceneSpacePos - reflections[reflection_index].position;

					for (int i = 0; i < reflections[reflection_index].iterations; i++)
					{
						float depth_sample = texture(reflection_data_cubemaps[reflection_index * DATA_TEX_NUM], sample_vector).g;
						if (depth_sample == 1.0f)
						{
							i = reflections[reflection_index].iterations; //exit loop
						}
						else
						{
							depth_sample = (depth_sample * sample_space_length) + reflections[reflection_index].clip_near;
							sample_vector = (normalize(sample_vector) * depth_sample) + offset;
						}
					}

					reflection_colour = (texture(reflection_data_cubemaps[reflection_index * DATA_TEX_NUM], sample_vector).g == 1.0f) ? texture(skyboxTexture, sample_vector).rgb : texture(reflection_cubemaps[reflection_index], sample_vector).rgb;
				}
				else if (reflections[reflection_index].mode == ReflectionModeOBB) //oriented bounding box
				{
					vec3 all_intersections[2 * APPROXIMATION_OBB_NUM]; //2d array with start and end positions of all line segments
					int search_index = -1; //index of a line segment that contains the point of reflection
					vec3 refl_dir = normalize(reflect(-fragtocam, normal)); //direction to cast the rays in
					bool valid_segments[APPROXIMATION_OBB_NUM]; //whether or not each pair in all_intersections is a valid line segment or just junk data

					for (int i = 0; i < APPROXIMATION_OBB_NUM; i++)
					{
						bool is_valid; //whether or not the line segment is junk data
						vec3 intersections[2]; //start and end of line segment
						GetFirstOBBIntersection(geomSceneSpacePos, refl_dir, scene_approximations[i].position, scene_approximations[i].dimensions, scene_approximations[i].rotation, scene_approximations[i].rotation_inverse, is_valid, intersections); //find where (if anywhere) the reflection passes through this specific OBB

						//make sure that the line segment is aligned with the reflection vector (i.e. 0 -> 1 is aligned)
						if (dot(intersections[1] - intersections[0], refl_dir) < 0.0f)
						{
							vec3 swap = intersections[0];
							intersections[0] = intersections[1];
							intersections[1] = swap;
						}

						//write OBB intersection info into proper storage locations
						all_intersections[i * 2] = intersections[0];
						all_intersections[(i * 2) + 1] = intersections[1];

						valid_segments[i] = is_valid;

						if (is_valid)
						{
							//check if the line segment overlaps the reflection point (i.e. starts on one side and ends on the other)
							const float tolerance = 0.01f;
							const vec3 tolerance_vec = tolerance * refl_dir;
							const bool intersection_0_before_point = dot(intersections[0] - geomSceneSpacePos - tolerance_vec, refl_dir) < 0.0f;
							const bool intersection_1_after_point = dot(intersections[1] - geomSceneSpacePos + tolerance_vec, refl_dir) > 0.0f;

							if (intersection_0_before_point && intersection_1_after_point)
							{
								search_index = i;
							}
						}
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
							bool clear_run = false;
							while (!clear_run)
							{
								clear_run = true; //changes to false whenever a line segment is merged in that affects the end point

								for (int i = 0; i < APPROXIMATION_OBB_NUM; i++)
								{
									if (valid_segments[i] && !included_segments[i]) //don't check line segments that are already included in the line
									{
										//check if extending end is possible
										const float tolerance = 0.01f;
										const vec3 tolerance_vec = tolerance * refl_dir;

										// (of the existing line segment)
										const bool starts_after_start = dot(all_intersections[i * 2] - line_start + tolerance_vec, refl_dir) > 0.0f;
										const bool starts_before_end = dot(all_intersections[i * 2] - line_end - tolerance_vec, refl_dir) < 0.0f;
										const bool ends_after_end = dot(all_intersections[(i * 2) + 1] - line_end - tolerance_vec, refl_dir) > 0.0f;

										if (starts_after_start && starts_before_end && ends_after_end) //new line segment overlaps with end of current line segment - extend line
										{
											clear_run = false;
											included_segments[i] = true;

											line_end = all_intersections[(i * 2) + 1];
											line_end_index = i;
										}
									}
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
								if (new_length < current_length)
								{
									reflection_index = i;
									current_length = new_length;
								}
							}
						}
				
						//sample using the final values
						{
							vec3 sample_vector = line_end - reflections[reflection_index].position;
							vec4 reflection_sample = texture(reflection_cubemaps[reflection_index], sample_vector);

							reflection_colour = (texture(reflection_data_cubemaps[reflection_index * DATA_TEX_NUM], sample_vector).g == 1.0f)
							? texture(skyboxTexture,  refl_dir).rgb
							: reflection_sample.rgb;
						}
					}
				}
			}
			else
			{
				reflection_intensity = vec3(0.0f);
				reflection_colour = vec3(0.0f);
			}
		}
	}

	frag_out += vec4(reflection_intensity * reflection_colour, 0.0f);

	//apply skybox
	vec3 skybox_intensity = texture(skyboxMaskTexture, geomUV).rgb;
	frag_out *= vec4(1.0f - skybox_intensity, 1.0f);
	frag_out += vec4(skybox_intensity * texture(skyboxTexture, geomSceneSpacePos + cam_translate.xyz).rgb, 0.0f);
	frag_out.a = 1.0f;
	
	//texture usage:
	// colour: all 4 channels assigned, alpha is currently ignored
	// depth: left to opengl
	// data:
	//    0:
	//      r: 1 or 0: whether or not fragment should be shown in screen space reflections
	//      g: pseudo-depth - fragment depth except if the fragment is part of the skybox, in which case the depth is 1 (as far away as possible)

	//output whether or not to draw reflections on certain fragments in the next frame
	data_out[0].r = mat_ssr_show_this ? 1.0f : 0.0f;

	//store the pseudo-depth (depth accounting for skyboxes)
	data_out[0].g = (skybox_intensity == vec3(1.0f, 1.0f, 1.0f)) ? 1.0f : gl_FragCoord.z;
}