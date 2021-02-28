#version 430 core

#if !defined(NUM_TEXTURES)
#define NUM_TEXTURES 1
#endif

layout(location = 0) out vec4 colour_out[NUM_TEXTURES];

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
	colour_out[0] = texture(colourTexture, geomUV);
}