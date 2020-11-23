#version 430 core

#if !defined(DATA_TEX_NUM)
#define DATA_TEX_NUM 1
#endif

layout(location = 0) out vec4 frag_out;
layout(location = 1) out vec4 data_out[DATA_TEX_NUM];

in vec4 geomMdlSpacePos;
in vec4 geomSceneSpacePos;
in vec4 geomCamSpacePos;
in vec3 geomTangentSpacePos;

in vec2 geomUV;

in vec4 geomMdlSpaceNormal;
in vec4 geomSceneSpaceNormal;
in vec4 geomCamSpaceNormal;

in mat3 geomNormalTBN;

in vec3 geomTangentSpaceCameraPos;


uniform sampler2D colourTexture;

uniform vec3 sun_angle;


void main()
{
	frag_out = texture(colourTexture, geomUV);
}