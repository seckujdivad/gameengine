#version 400 core
out vec4 gl_FragColor;
in vec3 vpos;

void main()
{
	//gl_FragColor = vec4(1.0f, 0.5f, 0.5f, 1.0f);
	//gl_FragColor = vec4(vpos.xyz, 1.0f);
	gl_FragColor = vec4(vpos.x, vpos.x, vpos.x, 1.0f);
}