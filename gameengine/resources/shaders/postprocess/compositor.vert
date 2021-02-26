#version 400 core
layout (location = 0) in vec3 inPos;
layout (location = 2) in vec2 inUV;

out vec2 globalUV;

void main()
{
	globalUV = inUV;

	gl_Position = vec4(inPos, 1.0f);
}