#version 430 core

#if !defined(DATA_TEX_NUM)
#define DATA_TEX_NUM 1
#endif

layout(location = 0) out vec4 frag_out;
layout(location = 1) out vec4 data_out[DATA_TEX_NUM];

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


void main()
{
	frag_out = texture(colourTexture, geomUV);
}