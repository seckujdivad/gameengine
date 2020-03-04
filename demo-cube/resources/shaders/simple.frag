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

//textures
uniform sampler2D colourTexture;

//lighting
uniform vec3 light_ambient;

struct PointLight
{
	vec3 position;
	vec3 diffuse;
	vec3 specular;
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

	float diffuse_intensity;
	vec3 fragtolight;

	float specular_intensity;
	vec3 fragtocam;
	vec3 perfect_reflection;

	for (int i = 0; i < POINT_LIGHT_NUM; i++)
	{
		fragtolight = normalize(light_points[i].position - globalSceneSpacePos.xyz);
		diffuse_intensity = max(dot(normal, fragtolight), 0.0f);
		frag_intensity = frag_intensity + (diffuse_intensity * light_points[i].diffuse);

		fragtocam = normalize(vec3(cam_translate) - globalSceneSpacePos.xyz);
		perfect_reflection = reflect(0 - fragtolight, normal);
		specular_intensity = pow(max(dot(fragtocam, perfect_reflection), 0.0f), 32);
		frag_intensity = frag_intensity + (specular_intensity * light_points[i].specular);
	}

	//apply lighting to fragment
	frag_out = vec4(frag_intensity, 1.0f) * frag_out;
}