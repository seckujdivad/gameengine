#version 430 core

#if !defined(DATA_TEX_NUM)
#define DATA_TEX_NUM 1
#endif

#if defined(ACCESS_CUBE_MAPS)
#define INPUT_TEXTURE sampler2DArray
#else
#define INPUT_TEXTURE sampler2D
#endif

layout(location = 0) out vec4 frag_out;

in vec2 geomUV;

uniform INPUT_TEXTURE gbufferTextureSample;
uniform INPUT_TEXTURE gbufferLighting;
uniform INPUT_TEXTURE gbufferNormal;
uniform INPUT_TEXTURE gbufferSpecular;
uniform INPUT_TEXTURE gbufferDiffuse;

uniform vec2 screen_dimensions;

uniform mat4 cubemap_transform_inverse[6];

uniform mat4 cam_persp_inverse;

vec3 PerspDiv(vec4 vec)
{
	return vec.xyz / vec.w;
}

vec4 GetTexture(INPUT_TEXTURE tex, vec2 texel)
{
#if defined(ACCESS_CUBE_MAPS)
	return texture(tex, vec3(texel, gl_Layer));
#else
	return texture(tex, texel);
#endif
}

vec4 GetTexture(INPUT_TEXTURE tex)
{
	return GetTexture(tex, geomUV);
}

void main()
{
	frag_out = GetTexture(gbufferTextureSample);
	frag_out *= vec4(GetTexture(gbufferLighting).rgb, 1.0f);
}