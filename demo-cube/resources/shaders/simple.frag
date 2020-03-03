#version 400 core
layout(location = 0) out vec4 frag_out;
in vec3 globalPos;
in vec3 globalNormal;
in vec2 globalUV;

uniform sampler2D colourTexture;

void main()
{
	//frag_out = vec4(normalize(globalPos), 1.0f);
	//frag_out = vec4(vec3(gl_FragCoord.z).xyz, 1.0f);
	frag_out = texture(colourTexture, globalUV);
	//frag_out = vec4(globalUV.st, 0.0f, 1.0f);
}