#version 400 core

#if !defined(NUM_TEXTURES)
#define NUM_TEXTURES 1
#endif

#if !defined(NUM_NORMAL_DRAW_TEXTURES)
#define NUM_NORMAL_DRAW_TEXTURES 2
#endif

layout(location = 0) out vec4 colour_out[NUM_TEXTURES];

in vec2 globalUV;

uniform sampler2D draw_frame[NUM_NORMAL_DRAW_TEXTURES];
uniform sampler2D draw_frame_depth;

void main()
{
	const vec4 skybox_colour = vec4(vec3(0.0f), 0.0f); //transparent background

	float depth = texture(draw_frame_depth, globalUV).r;
	gl_FragDepth = depth;

	vec2 ssr_sample_uv = texture(draw_frame[1], globalUV).xy;

	colour_out[0] = depth == 1.0f ? skybox_colour : texture(draw_frame[0], ssr_sample_uv);
}