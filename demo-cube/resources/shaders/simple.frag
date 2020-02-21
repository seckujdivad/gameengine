#version 400 core
layout(location = 0) out vec4 frag_out;
in vec3 vpos;

void main()
{
	//gl_FragColor = vec4(0.5f, 0.5f, 0.5f, 1.0f);
	//gl_FragColor = vec4(vpos.xyz, 1.0f);
	frag_out = vec4(normalize(vpos), 1.0f);
	//frag_out = vec4(vec3(gl_FragCoord.z).xyz, 1.0f);
}