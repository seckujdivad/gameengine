#version 400 core

#if !defined(COMPOSITE_LAYER_NUM)
#define COMPOSITE_LAYER_NUM 1
#endif

layout(location = 0) out vec4 frag_out;

in vec2 globalUV;
in vec2 globalPosition;

uniform sampler2D textures[COMPOSITE_LAYER_NUM];

void main()
{
	frag_out = texture(textures[0], globalUV);

	for (int i = 1; i < COMPOSITE_LAYER_NUM; i++)
	{
		vec4 texture_sample = texture(textures[i], globalUV);

		if (texture_sample.a > 0.0f)
		{
			vec4 output_sample;
			output_sample.a = texture_sample.a + (frag_out.a * (1 - texture_sample.a));
			output_sample.rgb = ((texture_sample.rgb * texture_sample.a) + (frag_out.rgb * frag_out.a * (1 - texture_sample.a))) / output_sample.a;
			frag_out = output_sample;
		}
	}
}