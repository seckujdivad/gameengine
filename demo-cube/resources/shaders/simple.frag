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

uniform vec3 mat_diffuse;
uniform vec3 mat_specular;
uniform float mat_specular_highlight;

//textures
uniform sampler2D colourTexture;
uniform sampler2D normalTexture;

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
		return 0.0f;
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

	//apply normal map
	
	//calculate light intensity
	// ambient
	vec3 frag_intensity = vec3(0.0f);
	frag_intensity = frag_intensity + light_ambient;

	// diffuse
	vec3 sample_normal = texture(normalTexture, globalUV).rgb;
	sample_normal = normalize(2.0f * (sample_normal - 0.5f));

	vec3 normal = globalNormalTBN * sample_normal;

	//vars for loop
	float diffuse_intensity;
	float specular_intensity;
	vec3 fragtolight;
	vec3 fragtocam;

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
		fragtolight = normalize(light_points[i].position - globalSceneSpacePos.xyz); //get direction from the fragment to the light source
		fragtocam = normalize(vec3(cam_translate) - globalSceneSpacePos.xyz); //get direction from the fragment to the camera

		// diffuse
		diffuse_intensity = max(dot(normal, fragtolight), 0.0f); //calculate diffuse intensity, floor = 0
		light_change = diffuse_intensity * mat_diffuse * light_points[i].intensity; //apply diffuse intensity and material diffuse colour to fragment intensity
		
		// specular
		// phong
		//perfect_reflection = reflect(0 - fragtolight, normal); //calculate the direction that light bounces off the surface at
		//specular_intensity = pow(max(dot(fragtocam, perfect_reflection), 0.0f), mat_specular_highlight); //use angle between the ideal bounce direction and the bounce direction to reach the camera to get the intensity
		// blinn-phong
		halfway_dir = normalize(fragtolight + fragtocam);
		specular_intensity = pow(max(dot(normal, halfway_dir), 0.0f), mat_specular_highlight);
		light_change = light_change + (specular_intensity * mat_specular * light_points[i].intensity); //apply specular intensity and material specular colour to fragment intensity
		
		//apply light to fragment
		// get shadow intensity multiplier
		shadow_intensity = GetShadowIntensity(globalSceneSpacePos.xyz, i);
		frag_intensity = frag_intensity + (light_change * shadow_intensity);
	}

	//apply lighting to fragment
	frag_out = vec4(frag_intensity, 1.0f) * frag_out;
}