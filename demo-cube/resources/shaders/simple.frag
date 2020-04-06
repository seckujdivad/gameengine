#version 400 core

//directives to avoid editor showing errors
#if !defined(POINT_LIGHT_NUM)
#define POINT_LIGHT_NUM 1
#endif

//shader input-output
layout(location = 0) out vec4 frag_out;

in vec4 globalMdlSpacePos;
in vec4 globalSceneSpacePos;
in vec4 globalCamSpacePos;

in vec2 globalUV;

in vec4 globalMdlSpaceNormal;
in vec4 globalSceneSpaceNormal;
in vec4 globalCamSpaceNormal;

in mat3 globalNormalTBN;

uniform vec4 mdl_translate;
uniform mat4 mdl_rotate;
uniform mat4 mdl_scale;

uniform vec4 cam_translate;
uniform mat4 cam_rotate;
uniform mat4 cam_persp;
uniform float cam_clip_near;
uniform float cam_clip_far;

uniform vec3 mat_diffuse;
uniform vec3 mat_specular;
uniform float mat_specular_highlight;
uniform int mat_reflection_mode; //0: iterative, 1: obb

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

void main()
{
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

	//reflections
	vec3 reflection_intensity = texture(reflectionIntensityTexture, globalUV).rgb;
	vec3 reflection_colour = vec3(0.0f, 0.0f, 0.0f);
	if (reflection_intensity == vec3(0.0f, 0.0f, 0.0f))
	{
		//do nothing - reflections have no effect on this fragment
	}
	else if (mat_reflection_mode == 0) //iteratively apply perspective correction
	{
		vec3 sample_vector = reflect(-fragtocam, normal);
		float depth_sample;
		float sample_space_length = reflection_clip_far - reflection_clip_near;
		vec3 offset = globalSceneSpacePos.xyz - reflection_position;

		for (int i = 0; i < reflection_parallax_it_iterations; i++)
		{
			depth_sample = texture(reflection_cubemap, sample_vector).a;
			depth_sample = (depth_sample * sample_space_length) + reflection_clip_near;
			sample_vector = (normalize(sample_vector) * depth_sample) + offset;
		}

		reflection_colour = texture(reflection_cubemap, sample_vector).rgb;
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
				if (j == 0) //at an intersection, one of the values is pinned (at a minimum or maximum) while the other two are inside the range of their maximum and minimum
				{
					pinned_value = 0.0f;
				}
				else
				{
					pinned_value = aabb[i];
				}
				
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
		reflection_colour = texture(reflection_cubemap, intersection - reflection_position).rgb;
	}

	frag_intensity += reflection_intensity * reflection_colour;

	//apply lighting to fragment
	frag_out = vec4(frag_intensity, 1.0f) * frag_out;

	//if the shader is drawing a reflection cubemap, store the depth in the alpha channel
	if (reflection_isdrawing)
	{
		frag_out.a = gl_FragDepth;
	}
}