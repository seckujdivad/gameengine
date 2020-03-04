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

//lighting
uniform vec3 light_ambient;

struct PointLight
{
	vec3 position;
	vec3 intensity;
};

uniform PointLight light_points[POINT_LIGHT_NUM];

void main()
{
	//get base colour
	frag_out = texture(colourTexture, globalUV);
	
	//calculate light intensity
	// ambient
	vec3 frag_intensity = vec3(0.0f);
	frag_intensity = frag_intensity + light_ambient;

	// diffuse
	vec3 normal = normalize(globalSceneSpaceNormal.xyz);

	//vars for loop
	float diffuse_intensity;
	float specular_intensity;
	vec3 fragtolight;
	vec3 fragtocam;
	vec3 perfect_reflection;

	for (int i = 0; i < POINT_LIGHT_NUM; i++)
	{
		fragtolight = normalize(light_points[i].position - globalSceneSpacePos.xyz); //get direction from the fragment to the light source
		fragtocam = normalize(vec3(cam_translate) - globalSceneSpacePos.xyz); //get direction from the fragment to the camera

		//diffuse
		diffuse_intensity = max(dot(normal, fragtolight), 0.0f); //calculate diffuse intensity, floor = 0
		frag_intensity = frag_intensity + (diffuse_intensity * mat_diffuse * light_points[i].intensity); //apply diffuse intensity and material diffuse colour to fragment intensity
		
		//specular
		perfect_reflection = reflect(0 - fragtolight, normal); //calculate the direction that light bounces off the surface at
		specular_intensity = pow(max(dot(fragtocam, perfect_reflection), 0.0f), mat_specular_highlight); //use angle between the ideal bounce direction and the bounce direction to reach the camera to get the intensity
		frag_intensity = frag_intensity + (specular_intensity * mat_specular * light_points[i].intensity); //apply specular intensity and material specular colour to fragment intensity
	}

	//apply lighting to fragment
	frag_out = vec4(frag_intensity, 1.0f) * frag_out;
}