#version 400 core
out vec4 gl_FragColor;
in vec3 vpos;

void main()
{
	//gl_FragColor = vec4(1.0f, 0.5f, 0.5f, 1.0f);
	//gl_FragColor = vec4(vpos.xyz, 1.0f);
	float divisor = 2.0f;
	gl_FragColor = vec4(normalize(vpos), 1.0f);
}