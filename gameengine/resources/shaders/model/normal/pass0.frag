#version 430 core

#if !defined(PASSTHROUGH_TEX_NUM)
#define PASSTHROUGH_TEX_NUM 4
#endif

layout(location = 0) out vec4 data_out[PASSTHROUGH_TEX_NUM];

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

uniform sampler2D colourTexture;
uniform sampler2D specularTexture;

uniform vec3 mat_diffuse;
uniform float mat_specular_highlight;

void main()
{
	data_out[0] = texture(colourTexture, geomUV);

	data_out[1].rgb = (geomSceneSpaceNormal / 2.0f) + 0.5f;
	data_out[1].a = mat_specular_highlight / 100.0f;

	data_out[2].rgb = texture(specularTexture, geomUV).rgb;

	data_out[3].rgb = mat_diffuse;
}