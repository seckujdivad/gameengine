#version 400 core

//directives to avoid editor showing errors
#if !defined(NUM_TEXTURES)
#define NUM_TEXTURES 1
#endif

//shader input-output
layout(location = 0) out vec4 colour_out[NUM_TEXTURES];
uniform vec3 wireframe_colour;

void main()
{
	colour_out[0].rgb = wireframe_colour;
	colour_out[0].a = 1.0f;
}