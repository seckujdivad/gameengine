#version 400 core
layout (location = 0) in vec3 inPos;

out vec2 globalUV;

void main()
{
	globalUV = inPos.xy;
	globalUV.x = (globalUV.x + 1.0f) / 2.0f;
	globalUV.y = (globalUV.y + 1.0f) / 2.0f;

	gl_Position = vec4(inPos, 1.0f);
}