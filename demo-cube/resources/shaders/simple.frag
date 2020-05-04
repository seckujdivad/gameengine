#version 400 core

//directives to avoid editor showing errors
#if !defined(POINT_LIGHT_NUM)
#define POINT_LIGHT_NUM 1
#endif

#if !defined(DATA_TEX_NUM)
#define DATA_TEX_NUM 1
#endif

//shader input-output
layout(location = 0) out vec4 frag_out;
layout(location = 1) out vec4 data_out[DATA_TEX_NUM];

in vec4 globalMdlSpacePos;
in vec4 globalSceneSpacePos;
in vec4 globalCamSpacePos;

in vec2 globalUV;

in vec4 globalMdlSpaceNormal;
in vec4 globalSceneSpaceNormal;
in vec4 globalCamSpaceNormal;

in mat3 globalNormalTBN;

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
uniform int mat_reflection_mode; //0: iterative, 1: obb

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
	bool shadows_enabled;
	samplerCube shadow_cubemap;
	float shadow_far_plane;
	float shadow_bias;
};

uniform PointLight light_points[POINT_LIGHT_NUM];

//reflections
uniform vec3 reflection_position;
uniform samplerCube reflection_cubemap;
uniform bool reflection_isdrawing;
uniform float reflection_clip_near;
uniform float reflection_clip_far;
uniform int reflection_parallax_it_iterations;
uniform vec3 reflection_parallax_obb_position;
uniform vec3 reflection_parallax_obb_dimensions;
uniform mat3 reflection_parallax_obb_rotation;
uniform mat3 reflection_parallax_obb_rotation_inverse;

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

float GetShadowIntensity(vec3 fragpos, int lightindex)
{
	if (!light_points[lightindex].shadows_enabled)
	{
		return 1.0f;
	}

	vec3 lighttofrag = fragpos - light_points[lightindex].position;
	float depth_sample = texture(light_points[lightindex].shadow_cubemap, lighttofrag).r;
	float corrected_depth_sample = depth_sample * light_points[lightindex].shadow_far_plane;
	float frag_depth = length(lighttofrag);
	
	if (depth_sample == 1.0f)
	{
		if (frag_depth < light_points[lightindex].shadow_far_plane)
		{
			return 1.0f;
		}
		else
		{
			return 0.0f;
		}
	}
	else if (frag_depth - light_points[lightindex].shadow_bias > corrected_depth_sample)
	{
		return 0.0f;
	}
	else
	{
		return 1.0f;
	}
}

float NDCZToCamZ(float ndc_z)
{
	return (ndc_z * (cam_clip_far - cam_clip_near)) + cam_clip_near;
	//return (2.0f * cam_clip_far * cam_clip_near) / ((ndc_z / (cam_clip_far - cam_clip_near)) - cam_clip_far - cam_clip_near);
}

float CamZToNDCZ(float cam_z)
{
	return (cam_z - cam_clip_near) / (cam_clip_far - cam_clip_near);
	//return cam_z * ((cam_clip_near + cam_clip_far) / (2.0f * cam_clip_near * cam_clip_far));
}

void main()
{
	for (int i = 0; i < DATA_TEX_NUM; i++)
	{
		data_out[0] = vec4(0.0f);
	}

	//get base colour
	frag_out = texture(colourTexture, globalUV);

	//apply ambient light
	vec3 frag_intensity = vec3(0.0f);
	frag_intensity = frag_intensity + light_ambient;

	//calculate local mapped normal
	vec3 sample_normal = texture(normalTexture, globalUV).rgb;
	sample_normal = normalize(2.0f * (sample_normal - 0.5f));
	vec3 normal = normalize(globalNormalTBN * sample_normal);

	//sample specular map
	vec3 sample_specular = texture(specularTexture, globalUV).rgb * mat_specular;

	//vars for loop
	float diffuse_intensity;
	float specular_intensity;
	vec3 fragtolight;
	vec3 fragtocam = normalize(0 - vec3(cam_translate) - globalSceneSpacePos.xyz); //get direction from the fragment to the camera

	//phong
	//vec3 perfect_reflection;
	//blinn-phong
	vec3 halfway_dir;

	vec3 light_change;

	//shadows
	float shadow_intensity;

	for (int i = 0; i < POINT_LIGHT_NUM; i++)
	{
		if (light_points[i].intensity != vec3(0.0f, 0.0f, 0.0f))
		{
			//calculate light
			light_change = vec3(0.0f);

			fragtolight = light_points[i].position - globalSceneSpacePos.xyz; //get direction from the fragment to the light source

			if (length(fragtolight) < light_points[i].shadow_far_plane) //make sure fragment isn't too far away from the light
			{
				fragtolight = normalize(fragtolight);

				// diffuse
				diffuse_intensity = max(dot(normal, fragtolight), 0.0f); //calculate diffuse intensity, floor = 0
				light_change = diffuse_intensity * mat_diffuse * light_points[i].intensity; //apply diffuse intensity and material diffuse colour to fragment intensity
		
				// specular blinn-phong
				halfway_dir = normalize(fragtolight + fragtocam);
				specular_intensity = pow(max(dot(normal, halfway_dir), 0.0f), mat_specular_highlight);
				light_change += specular_intensity * sample_specular * light_points[i].intensity; //apply specular intensity and material specular colour to fragment intensity
		
				//get shadow intensity multiplier
				shadow_intensity = GetShadowIntensity(globalSceneSpacePos.xyz, i);

				//apply light to fragment
				frag_intensity = frag_intensity + (light_change * shadow_intensity);
			}
		}
	}

	//apply lighting to fragment
	frag_out = vec4(frag_intensity, 1.0f) * frag_out;

	//reflections
	vec3 reflection_intensity = texture(reflectionIntensityTexture, globalUV).rgb;
	vec3 reflection_colour = vec3(0.0f, 0.0f, 0.0f);
	if (reflection_intensity == vec3(0.0f, 0.0f, 0.0f))
	{
		//do nothing - reflections have no effect on this fragment
	}
	else
	{
		bool ssr_reflection_applied = false;
		if (render_output_valid)
		{
			if ((length(globalCamSpacePos.xyz) < mat_ssr_max_distance))
			{
				vec3 direction = normalize(reflect(-fragtocam, normal));
				vec3 start_pos = globalSceneSpacePos.xyz;
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

				float hit_increment = abs((mat_ssr_resolution * 2.0f * float(2 << (search_level - 1))) / ((ss_direction.x > ss_direction.y) ? (render_output_x * ss_direction.x) : (render_output_y * ss_direction.y)));
				hit_increment = max(hit_increment, 1.0f / 500.0f);

				vec3 ss_position;
				float sample_depth;
				vec2 tex_pos;
				float hit_pos = 0.0f;
				
				bool hit_detected;

				while (!ssr_reflection_applied && all(greaterThan(ss_position, vec3(-1.0f))) && all(lessThan(ss_position, vec3(1.0f))) && (hit_pos < 1.0f))
				{
					ss_position.xy = mix(ss_start_pos.xy, ss_end_pos.xy, hit_pos);
					tex_pos = (ss_position.xy * 0.5f) + 0.5f;
					ss_position.z = 1 / mix(1 / ss_start_pos.z, 1 / ss_end_pos.z, hit_pos);

					sample_depth = (texture(render_output_depth, tex_pos.xy).r * 2.0f) - 1.0f;

					hit_detected = (texture(render_output_data[0], tex_pos.xy).r > 0.5f) && (abs(sample_depth - ss_position.z) * (cam_clip_far - cam_clip_near) * 0.5f < mat_ssr_depth_acceptance);

					if (hit_detected && (search_level == 0))
					{
						reflection_colour = texture(render_output_colour, tex_pos.xy).rgb;
						ssr_reflection_applied = true;
					}
					else if (hit_detected)
					{
						hit_pos = hit_pos - hit_increment;
						hit_increment = hit_increment / 8;
						search_level--;
					}

					hit_pos = hit_pos + hit_increment;
				}
			}
		}

		if (!ssr_reflection_applied)
		{
			if (mat_reflection_mode == 0) //iteratively apply perspective correction
			{
				vec3 sample_vector = reflect(-fragtocam, normal);
				float depth_sample;
				float sample_space_length = reflection_clip_far - reflection_clip_near;
				vec3 offset = globalSceneSpacePos.xyz - reflection_position;

				for (int i = 0; i < reflection_parallax_it_iterations; i++)
				{
					depth_sample = texture(reflection_cubemap, sample_vector).a;
					if (depth_sample == 1.0f)
					{
						i = reflection_parallax_it_iterations; //exit loop
					}
					else
					{
						depth_sample = (depth_sample * sample_space_length) + reflection_clip_near;
						sample_vector = (normalize(sample_vector) * depth_sample) + offset;
					}
				}

				reflection_colour = (texture(reflection_cubemap, sample_vector).a == 1.0f) ? texture(skyboxTexture, sample_vector).rgb : texture(reflection_cubemap, sample_vector).rgb;
			}
			else if (mat_reflection_mode == 1) //oriented bounding box
			{
		
				mat3 oob_rotation = reflection_parallax_obb_rotation;
				mat3 oob_rotation_inverse = reflection_parallax_obb_rotation_inverse;
				vec3 aabb = reflection_parallax_obb_dimensions; //axis aligned bounding box

				vec3 oob_translation = reflection_parallax_obb_position - (oob_rotation * 0.5f * reflection_parallax_obb_dimensions);
				vec3 reflection = normalize(reflect(-fragtocam, normal));
				vec3 fragpos_oob = oob_rotation_inverse * (globalSceneSpacePos.xyz - oob_translation);
				vec3 reflection_oob = oob_rotation_inverse * reflection;
		
				vec3 intersection;
				float lambda;
				float pinned_value;
				float second_axis_value;
				float third_axis_value;
				for (int i = 0; i < 3; i++)
				{
					for (int j = 0; j < 2; j++)
					{
						//at an intersection, one of the values is pinned (at a minimum or maximum) while the other two are inside the range of their maximum and minimum
						pinned_value = (j == 0) ? 0.0f : aabb[i];
				
						lambda = (pinned_value - fragpos_oob[i]) / reflection_oob[i];
						if (lambda > 0.1) //direction vector is normalised, so length is equal to lambda
						{
							second_axis_value = fragpos_oob[(i + 1) % 3] + (lambda * reflection_oob[(i + 1) % 3]);
							third_axis_value = fragpos_oob[(i + 2) % 3] + (lambda * reflection_oob[(i + 2) % 3]);
							if ((0.0f <= second_axis_value) && (second_axis_value <= aabb[(i + 1) % 3]) &&
								(0.0f <= third_axis_value) && (third_axis_value <= aabb[(i + 2) % 3]))
							{
								intersection[i] = pinned_value;
								intersection[(i + 1) % 3] = second_axis_value;
								intersection[(i + 2) % 3] = third_axis_value;
							}
						}
					}
				}

				intersection = (oob_rotation * intersection) + oob_translation;
		
				//sample using the final values
				vec3 sample_vector = intersection - reflection_position;
				vec4 reflection_sample = texture(reflection_cubemap, sample_vector).rgba;

				reflection_colour = (reflection_sample.a == 1.0f) ? texture(skyboxTexture, reflection).rgb : reflection_sample.rgb;
			}
		}
	}

	frag_out += vec4(reflection_intensity * reflection_colour, 0.0f);

	//apply skybox
	vec3 skybox_intensity = texture(skyboxMaskTexture, globalUV).rgb;
	if (skybox_intensity != vec3(0.0f, 0.0f, 0.0f))
	{
		frag_out = vec4(1 - skybox_intensity, 1.0f) * frag_out;
		frag_out += vec4(skybox_intensity * texture(skyboxTexture, globalSceneSpacePos.xyz + cam_translate.xyz).rgb, 0.0f);
	}

	//if the shader is drawing a reflection cubemap, store the depth in the alpha channel
	if (reflection_isdrawing)
	{
		frag_out.a = (skybox_intensity == vec3(1.0f, 1.0f, 1.0f)) ? 1.0f : gl_FragDepth;
	}
	
	//output whether or not to draw reflections on certain fragments in the next frame
	data_out[0].r = mat_ssr_show_this ? 1.0f : 0.0f;
}