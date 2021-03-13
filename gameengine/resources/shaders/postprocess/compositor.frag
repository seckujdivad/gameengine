#version 400 core

#if !defined(COMPOSITE_LAYER_NUM)
#define LAYER_NUM 1
#endif

#if !defined(NUM_TEXTURES)
#define NUM_TEXTURES 1
#endif

layout(location = 0) out vec4 colour_out[NUM_TEXTURES];

in vec2 geomUV;

uniform sampler2D layers_texture[LAYER_NUM];

struct Layer
{
	vec4 colour_translate;
	vec4 colour_scale;
};
uniform Layer layers[LAYER_NUM];

//post process mode
const int modeAlphaBlend = 0;
uniform int mode;

void main()
{
	if (mode == modeAlphaBlend)
	{
		colour_out[0] = vec4(vec3(1.0f), 1.0f);

		for (int i = 0; i < LAYER_NUM; i++)
		{
			vec4 texture_sample = layers[i].colour_translate + (texture(layers_texture[i], geomUV) * layers[i].colour_scale);

			if (texture_sample.a > 0.0f)
			{
				vec4 output_sample;
				output_sample.a = texture_sample.a + (colour_out[0].a * (1 - texture_sample.a));
				output_sample.rgb = ((texture_sample.rgb * texture_sample.a) + (colour_out[0].rgb * colour_out[0].a * (1 - texture_sample.a))) / output_sample.a;
				colour_out[0] = output_sample;
			}
		}
	}
}