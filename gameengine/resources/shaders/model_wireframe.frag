#version 430 core

//directives to avoid editor showing errors
#if !defined(DATA_TEX_NUM)
#define DATA_TEX_NUM 1
#endif

//shader input-output
layout(location = 0) out vec4 frag_out;
layout(location = 1) out vec4 data_out[DATA_TEX_NUM];

in vec4 geomMdlSpacePos;
in vec4 geomSceneSpacePos;
in vec4 geomCamSpacePos;

in vec2 geomUV;

in vec4 geomMdlSpaceNormal;
in vec4 geomSceneSpaceNormal;
in vec4 geomCamSpaceNormal;

in mat3 geomNormalTBN;

uniform vec3 wireframe_colour;

void main()
{
	frag_out.rgb = wireframe_colour;
}