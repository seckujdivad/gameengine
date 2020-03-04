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
	vec3 lightpos;

	float diffuse_intensity;
	float dotproduct;

	for (int i = 0; i < POINT_LIGHT_NUM; i++)
	{
		lightpos = light_points[i].position;

		dotproduct = dot(normal, normalize(lightpos - globalSceneSpacePos.xyz));
		diffuse_intensity = max(dotproduct, 0.0f);

		frag_intensity = frag_intensity + (diffuse_intensity * light_points[i].diffuse);
	}

	//apply lighting to fragment
	frag_out = vec4(frag_intensity, 1.0f) * frag_out;
}