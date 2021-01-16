#version 400 core
layout (location = 0) in vec3 inPos;
layout (location = 2) in vec2 inUV;

out vec2 ssUV;

void main()
{
	ssUV = inUV;

	gl_Position = vec4(inPos, 1.0f);
}