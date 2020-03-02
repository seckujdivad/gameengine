#version 400 core
layout(location = 0) out vec4 frag_out;
in vec3 inPos;
in vec3 inNormal;
in vec2 inUV;

void main()
{
	//gl_FragColor = vec4(0.5f, 0.5f, 0.5f, 1.0f);
	//gl_FragColor = vec4(vpos.xyz, 1.0f);
	frag_out = vec4(normalize(inPos), 1.0f);
	//frag_out = vec4(vec3(gl_FragCoord.z).xyz, 1.0f);
}