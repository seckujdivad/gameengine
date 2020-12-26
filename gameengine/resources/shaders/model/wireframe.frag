#version 400 core

//directives to avoid editor showing errors
#if !defined(DATA_TEX_NUM)
#define DATA_TEX_NUM 1
#endif

//shader input-output
layout(location = 0) out vec4 frag_out;
layout(location = 1) out vec4 data_out[DATA_TEX_NUM];

uniform vec3 wireframe_colour;

void main()
{
	frag_out.rgb = wireframe_colour;
	frag_out.a = 1.0f;
}