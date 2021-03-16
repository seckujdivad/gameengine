#version 430 core

#if !defined(COMPOSITE_LAYER_NUM)
#define LAYER_NUM 1
#endif

#if !defined(NUM_TEXTURES)
#define NUM_TEXTURES 1
#endif

#if TARGET_IS_CUBEMAP == 1
#define TARGET_TYPE samplerCube
#else
#define TARGET_TYPE sampler2D
#endif

layout(location = 0) out vec4 colour_out[NUM_TEXTURES];

in vec2 geomUV;

uniform TARGET_TYPE layers_texture[LAYER_NUM];

struct Layer
{
	vec4 colour_translate;
	vec4 colour_scale;
};
uniform Layer layers[LAYER_NUM];

//post process mode
const int modeAlphaBlend = 0;
const int modeBoxBlur = 1;
uniform int mode;

uniform struct
{
	ivec2 radius;
	bool is_first_pass;
} modedata_BoxBlur;

vec4 SampleTarget(TARGET_TYPE to_sample, vec2 coords)
{
#if TARGET_IS_CUBEMAP == 1
	vec3 cubemap_coords = vec3((coords - 0.5f) * 2.0f, 1.0f);

	const vec3 axis_mults[6] = vec3[6](
		vec3(-1.0f, -1.0f, 1.0f), //x+: flip x and y (face space), +x face
		vec3(1.0f, -1.0f, -1.0f), //x-
		vec3(1.0f, 1.0f, 1.0f), //y+
		vec3(1.0f, -1.0f, -1.0f), //y-
		vec3(1.0f, -1.0f, 1.0f), //z+
		vec3(-1.0f, -1.0f, -1.0f) //z-
	);

	const ivec3 axis_remaps[6] = ivec3[6](
		ivec3(2, 1, 0), //x+: res x = in z, res y = in y, res z = in x
		ivec3(2, 1, 0), //x-
		ivec3(0, 2, 1), //y+
		ivec3(0, 2, 1), //y-
		ivec3(0, 1, 2), //z+
		ivec3(0, 1, 2) //z-
	);

	//mult is applied first, then remap
	vec3 cubemap_coords_transformed = vec3(0.0f);
	for (int i = 0; i < 3; i++)
	{
		int index = axis_remaps[gl_Layer][i];
		cubemap_coords_transformed[i] = cubemap_coords[index] * axis_mults[gl_Layer][index];
	}

	return texture(to_sample, clamp(cubemap_coords_transformed, vec3(-1.0f), vec3(1.0f)));
#else
	return texture(to_sample, coords);
#endif
}

void main()
{
	if (mode == modeAlphaBlend)
	{
		colour_out[0] = vec4(vec3(1.0f), 1.0f);

		for (int i = 0; i < LAYER_NUM; i++)
		{
			vec4 texture_sample = layers[i].colour_translate + (SampleTarget(layers_texture[i], geomUV) * layers[i].colour_scale);

			if (texture_sample.a > 0.0f)
			{
				vec4 output_sample;
				output_sample.a = texture_sample.a + (colour_out[0].a * (1 - texture_sample.a));
				output_sample.rgb = ((texture_sample.rgb * texture_sample.a) + (colour_out[0].rgb * colour_out[0].a * (1 - texture_sample.a))) / output_sample.a;
				colour_out[0] = output_sample;
			}
		}
	}
	else if (mode == modeBoxBlur)
	{
	}
}