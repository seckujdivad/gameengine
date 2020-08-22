#version 400 core
layout(location = 0) out vec4 frag_out;

in vec2 globalUV;
in vec2 globalPosition;

uniform sampler2D render_output;

void main()
{
	vec2 sample_position = globalPosition;

	frag_out = texture(render_output, (sample_position + 1.0f) * 0.5f).rgba;
}