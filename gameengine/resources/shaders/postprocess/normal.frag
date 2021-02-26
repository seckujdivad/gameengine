#version 430 core

#if !defined(NUM_TEXTURES)
#define NUM_TEXTURES 1
#endif

#if !defined(NUM_NORMAL_DRAW_TEXTURES)
#define NUM_NORMAL_DRAW_TEXTURES 2
#endif

#if !defined(TARGET_IS_CUBEMAP)
#define TARGET_IS_CUBEMAP 1
#endif

#if TARGET_IS_CUBEMAP == 1
#define TARGET_TYPE samplerCube
#else
#define TARGET_TYPE sampler2D
#endif

layout(location = 0) out vec4 colour_out[NUM_TEXTURES];

in vec2 geomUV;

uniform TARGET_TYPE draw_frame[NUM_NORMAL_DRAW_TEXTURES];
uniform TARGET_TYPE draw_frame_depth;

vec4 SampleTarget(TARGET_TYPE to_sample, vec2 coords)
{
#if TARGET_IS_CUBEMAP == 1
	vec3 cubemap_coords = vec3((coords - 0.5f) * 2.0f, 1.0f);

	//mult is applied first, then remap
	vec3 axis_mult = vec3(1.0f);
	ivec3 axis_remap = ivec3(0, 1, 2);

	if (gl_Layer == 0) //x+
	{
		axis_mult = vec3(-1.0f, -1.0f, 1.0f); //flip x and y (face space), +x face
		axis_remap = ivec3(2, 1, 0); //res x = in z, res y = in y, res z = in x
	}
	else if (gl_Layer == 1) //x-
	{
		axis_mult = vec3(1.0f, -1.0f, -1.0f);
		axis_remap = ivec3(2, 1, 0);
	}
	else if (gl_Layer == 2) //y+
	{
		axis_mult = vec3(1.0f, 1.0f, 1.0f);
		axis_remap = ivec3(0, 2, 1);
	}
	else if (gl_Layer == 3) //y-
	{
		axis_mult = vec3(1.0f, -1.0f, -1.0f);
		axis_remap = ivec3(0, 2, 1);
	}
	else if (gl_Layer == 4) //z+
	{
		axis_mult = vec3(1.0f, -1.0f, 1.0f);
		axis_remap = ivec3(0, 1, 2);
	}
	else if (gl_Layer == 5) //z-
	{
		axis_mult = vec3(-1.0f, -1.0f, -1.0f);
		axis_remap = ivec3(0, 1, 2);
	}
	else
	{
		axis_mult = vec3(1.0f);
		axis_remap = ivec3(0, 1, 2);
	}

	vec3 cubemap_coords_transformed = vec3(0.0f);
	for (int i = 0; i < 3; i++)
	{
		int index = axis_remap[i];
		cubemap_coords_transformed[i] = cubemap_coords[index] * axis_mult[index];
	}

	return texture(to_sample, cubemap_coords_transformed);
	//return texture(to_sample, vec3(coords, gl_Layer));
#else
	return texture(to_sample, coords);
#endif
}

void main()
{
	const vec4 skybox_colour = vec4(vec3(0.0f), 0.0f); //transparent background

	float depth = SampleTarget(draw_frame_depth, geomUV).r;
	gl_FragDepth = depth;

	vec2 ssr_sample_uv = SampleTarget(draw_frame[1], geomUV).xy;

	colour_out[0] = depth == 1.0f ? skybox_colour : SampleTarget(draw_frame[0], ssr_sample_uv);
}