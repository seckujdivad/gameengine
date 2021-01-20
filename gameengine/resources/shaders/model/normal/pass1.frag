#version 400 core

#if !defined(DATA_TEX_NUM)
#define DATA_TEX_NUM 1
#endif

layout(location = 0) out vec4 frag_out;

in vec2 ssUV;

uniform sampler2D gbufferTextureSample;
uniform sampler2D gbufferLighting;
uniform sampler2D gbufferNormal;
uniform sampler2D gbufferSpecular;
uniform sampler2D gbufferDiffuse;

void main()
{
	frag_out = texture(gbufferTextureSample, ssUV);
	frag_out *= vec4(texture(gbufferLighting, ssUV).rgb, 1.0f);
}