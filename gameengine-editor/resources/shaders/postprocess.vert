#version 400 core
layout (location = 0) in vec3 inPos;

out vec2 globalUV;
out vec2 globalPosition;

void main()
{
	globalPosition = inPos.xy;
	globalUV = (globalPosition + 1.0f) / 2.0f;

	gl_Position = vec4(inPos, 1.0f);
}