#version 400 core
out vec4 gl_FragColor;
in vec3 vpos;

void main()
{
	gl_FragColor = vec4(vpos.xyz, 1.0f);
}