#version 400 core
layout (location = 0) in vec3 aPos;
out vec3 vpos;

uniform vec4 mdl_translate;
uniform mat4 mdl_rotate;
uniform vec4 cam_translate;
uniform mat4 cam_rotate;
uniform mat4 cam_persp;

void main()
{
	gl_Position = cam_persp * (cam_rotate * ((mdl_rotate * vec4(aPos.xyz, 1.0f)) + mdl_translate + cam_translate));
	vpos = aPos;
}