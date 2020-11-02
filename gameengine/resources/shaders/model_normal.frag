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

//shader input-output
layout(location = 0) out vec4 frag_out;
layout(location = 1) out vec4 data_out[DATA_TEX_NUM];

in vec4 geomMdlSpacePos;
in vec4 geomSceneSpacePos;
in vec4 geomCamSpacePos;

in vec2 geomUV;

in vec4 geomMdlSpaceNormal;
in vec4 geomSceneSpaceNormal;
in vec4 geomCamSpaceNormal;

in mat3 geomNormalTBN;

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

void main()
{
	for (int i = 0; i < DATA_TEX_NUM; i++)
	{
		data_out[i] = vec4(0.0f);
	}

	//get base colour
	frag_out = texture(colourTexture, geomUV);

	//apply ambient light
	vec3 frag_intensity = vec3(0.0f);
	frag_intensity = frag_intensity + light_ambient;

	//calculate local mapped normal
	vec3 sample_normal = texture(normalTexture, geomUV).rgb;
	sample_normal = normalize(2.0f * (sample_normal - 0.5f));
	vec3 normal = normalize(geomNormalTBN * sample_normal);

	//sample specular map
	vec3 sample_specular = texture(specularTexture, geomUV).rgb * mat_specular;

	vec3 fragtocam = normalize(0 - vec3(cam_translate) - geomSceneSpacePos.xyz); //get direction from the fragment to the camera

	for (int i = 0; i < POINT_LIGHT_NUM; i++)
	{
		//calculate light
		vec3 light_change = vec3(0.0f);

		vec3 fragtolight = normalize(light_points[i].position - geomSceneSpacePos.xyz); //get direction from the fragment to the light source

		// diffuse
		float diffuse_intensity = max(dot(normal, fragtolight), 0.0f); //calculate diffuse intensity, floor = 0
		light_change = diffuse_intensity * mat_diffuse * light_points[i].intensity; //apply diffuse intensity and material diffuse colour to fragment intensity
		
		// specular blinn-phong
		vec3 halfway_dir = normalize(fragtolight + fragtocam);
		float specular_intensity = pow(max(dot(normal, halfway_dir), 0.0f), mat_specular_highlight);
		light_change += specular_intensity * sample_specular * light_points[i].intensity; //apply specular intensity and material specular colour to fragment intensity

		//apply light to fragment
		frag_intensity = frag_intensity + (light_change * GetShadowIntensity(geomSceneSpacePos.xyz, i));
	}

	//apply lighting to fragment
	frag_out = vec4(frag_intensity, 1.0f) * frag_out;

	//reflections
	vec3 reflection_intensity = texture(reflectionIntensityTexture, geomUV).rgb;
	vec3 reflection_colour = vec3(0.0f, 0.0f, 0.0f);
	{
		bool ssr_reflection_applied = false;
		if (render_output_valid && mat_ssr_enabled)
		{
			if ((length(geomCamSpacePos.xyz) < mat_ssr_max_distance))
			{
				vec3 direction = normalize(reflect(-fragtocam, normal));
				vec3 start_pos = geomSceneSpacePos.xyz;
				vec3 end_pos = start_pos + (direction * mat_ssr_max_cast_distance);
				start_pos = start_pos + (direction * ((mat_ssr_depth_acceptance * 1.5f) / dot(direction, normal)));

				vec4 ss_start_pos_prediv = cam_transform * vec4(start_pos, 1.0f);
				vec3 ss_start_pos = ss_start_pos_prediv.xyz / ss_start_pos_prediv.w;

				vec4 ss_depth_acceptance_prediv = cam_transform * (normalize(vec4(vec3(1.0f), 0.0f)) * mat_ssr_depth_acceptance);
				float ss_depth_acceptance = length(ss_depth_acceptance_prediv.xyz / ss_depth_acceptance_prediv.w);

				vec4 ss_end_pos_prediv = cam_transform * vec4(end_pos, 1.0f);
				vec3 ss_end_pos = ss_end_pos_prediv.xyz / ss_end_pos_prediv.w;

				vec3 ss_direction = ss_end_pos - ss_start_pos;

				int search_level = mat_ssr_refinements; //number of additional searches to carry out

				float hit_increment = abs(
					(mat_ssr_resolution * 2.0f * float(2 << (search_level - 1))) / (
							(abs(ss_direction.x) > abs(ss_direction.y)) ? (render_output_x * ss_direction.x) : (render_output_y * ss_direction.y)
						)
					);
				hit_increment = max(hit_increment, 1.0f / 500.0f);

				vec3 ss_position;
				float hit_pos = 0.0f;

				while (!ssr_reflection_applied && all(greaterThan(ss_position, vec3(-1.0f))) && all(lessThan(ss_position, vec3(1.0f))) && (hit_pos < 1.0f))
				{
					ss_position.xy = mix(ss_start_pos.xy, ss_end_pos.xy, hit_pos);
					vec2 tex_pos = (ss_position.xy * 0.5f) + 0.5f;
					ss_position.z = 1 / mix(1 / ss_start_pos.z, 1 / ss_end_pos.z, hit_pos);

					float sample_depth = (texture(render_output_depth, tex_pos.xy).r * 2.0f) - 1.0f;

					bool hit_detected = (-1.0f < sample_depth) && (sample_depth < 1.0f);
					hit_detected = hit_detected && (texture(render_output_data[0], tex_pos.xy).r > 0.5f);
					hit_detected = hit_detected && (abs(sample_depth - ss_position.z) * (cam_clip_far - cam_clip_near) * 0.5f < mat_ssr_depth_acceptance);

					if (hit_detected && (search_level == 0))
					{
						reflection_colour = texture(render_output_colour, tex_pos.xy).rgb;
						ssr_reflection_applied = true;
					}
					else if (hit_detected)
					{
						hit_pos = hit_pos - hit_increment;
						hit_increment = hit_increment / 2;
						search_level--;
					}

					hit_pos = hit_pos + hit_increment;
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
					float current_distance = length(reflections[i].position - geomSceneSpacePos.xyz);

					bool condition = (refl_distance == 0.0f) || (current_distance < refl_distance);
					reflection_index = condition ? i : reflection_index;
					refl_distance = mix(int(condition), refl_distance, current_distance);
				}
			
				if (reflections[reflection_index].mode == ReflectionModeIterative) //iteratively apply perspective correction
				{
					vec3 sample_vector = reflect(-fragtocam, normal);
					float sample_space_length = reflections[reflection_index].clip_far - reflections[reflection_index].clip_near;
					vec3 offset = geomSceneSpacePos.xyz - reflections[reflection_index].position;

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
					float final_length = -1.0f; //length of furthest intersection
					int search_index = -1; //index of furthest intersection
					vec3 refl_dir = normalize(reflect(-fragtocam, normal)); //direction to cast the rays in
					bool included_segments[APPROXIMATION_OBB_NUM]; //whether or not each pair in all_intersections is a valid line segment or just junk data

					for (int i = 0; i < APPROXIMATION_OBB_NUM; i++)
					{
						bool is_valid; //whether or not the line segment is junk data
						vec3 intersections[2]; //start and end of line segment
						GetFirstOBBIntersection(geomSceneSpacePos.xyz, refl_dir, scene_approximations[i].position, scene_approximations[i].dimensions, scene_approximations[i].rotation, scene_approximations[i].rotation_inverse, is_valid, intersections); //find where (if anywhere) the reflection passes through this specific OBB

						//write OBB intersection info into proper storage locations
						all_intersections[i * 2] = intersections[0];
						all_intersections[(i * 2) + 1] = intersections[1];

						included_segments[i] = !is_valid;

						if (is_valid)
						{
							float current_length = length(intersections[1] - geomSceneSpacePos.xyz);

							if ((final_length == -1.0f) || ((final_length > current_length) && (current_length > 0.1f)))
							{
								final_length = current_length;
								search_index = i;
							}
						}
					}
				
					if (search_index == -1)
					{
						reflection_colour = GenerateErrorPattern(vec3(1.0f, 0.0f, 0.0f), vec3(1.0f)); //error state
					}
					else
					{
						//search all values and see if you can step on from
						included_segments[search_index] = true;
				
						vec3 line_start_pos = all_intersections[search_index * 2];
						vec3 line_end_pos = all_intersections[(search_index * 2) + 1];
						int current_index = 0;
						float current_length = length(line_end_pos - line_start_pos);
				
						while (current_index < APPROXIMATION_OBB_NUM)
						{
							if (!included_segments[current_index] && (length(all_intersections[current_index * 2] - line_start_pos) - 0.1f < current_length) && (length(all_intersections[(current_index * 2) + 1] - line_start_pos) > current_length))
							{
								line_end_pos = all_intersections[(current_index * 2) + 1];
								included_segments[current_index] = true;
								current_index = -1;
								current_length = length(line_end_pos - line_start_pos);
							}
							current_index++;
						}

						int final_index = reflection_index;
						float final_length = length(line_end_pos - reflections[reflection_index].position);

						for (int i = 0; i < reflection_count; i++)
						{
							current_length = length(line_end_pos - reflections[i].position);

							if (current_length < final_length)
							{
								final_index = i;
								final_length = current_length;
							}
						}
				
						//sample using the final values
						vec3 sample_vector = line_end_pos - reflections[final_index].position;
						vec4 reflection_sample = texture(reflection_cubemaps[final_index], sample_vector).rgba;

						reflection_colour = (texture(reflection_data_cubemaps[final_index * DATA_TEX_NUM], sample_vector).g == 1.0f) ? texture(skyboxTexture,  reflect(-fragtocam, normal)).rgb : reflection_sample.rgb;
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
	frag_out += vec4(skybox_intensity * texture(skyboxTexture, geomSceneSpacePos.xyz + cam_translate.xyz).rgb, 0.0f);
	
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
	data_out[0].g = (skybox_intensity == vec3(1.0f, 1.0f, 1.0f)) ? 1.0f : gl_FragDepth;
}