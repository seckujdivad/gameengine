#version 400 core

#if !defined(DATA_TEX_NUM)
#define DATA_TEX_NUM 1
#endif

layout(location = 0) out vec4 frag_out;

in vec2 ssUV;

uniform sampler2D gbufferTextureSample;

void main()
{
	frag_out = texture(gbufferTextureSample, ssUV);
}