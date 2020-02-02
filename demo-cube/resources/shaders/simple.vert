#version 400 core
layout (location = 0) in vec3 aPos;
out vec3 vpos;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main()
{
	gl_Position = projection * view * model * vec4(aPos.xyz, 1.0f);
	vpos = aPos;
}