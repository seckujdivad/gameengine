#version 400 core

//directives to avoid editor showing errors
#if !defined(POINT_LIGHT_NUM)
#define POINT_LIGHT_NUM 1
#endif

//shader input-output
layout(location = 0) out vec4 frag_out;
in vec3 globalPos;
in vec3 globalNormal;
in vec2 globalUV;

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
	vec3 frag_intensity = vec3(0.0f);
	frag_intensity = frag_intensity + light_ambient;

	//frag_out = vec4(frag_out.x * frag_intensity.x, frag_out.y * frag_intensity.y, frag_out.z * frag_intensity.z, frag_out.w);
	frag_out = vec4(frag_intensity, 1.0f) * frag_out;
}