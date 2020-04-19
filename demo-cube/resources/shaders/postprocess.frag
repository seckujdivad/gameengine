#version 400 core
layout(location = 0) out vec4 frag_out;

in vec2 globalUV;

uniform sampler2D render_output;

void main()
{
	gl_FragColor = texture(render_output, globalUV).rgba;
}