#version 430 core
layout (location = 0) in vec3 inPos;
layout (location = 2) in vec2 inUV;

out vec2 vertUV;
out vec4 vertPos;

void main()
{
	vertUV = inUV;

	vertPos = vec4(inPos, 1.0f);
}