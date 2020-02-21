#version 400 core
layout (location = 0) in vec3 aPos;
out vec3 vpos;

uniform vec4 mdl_translate;
uniform mat4 mdl_rotate;
uniform mat4 mdl_scale;

uniform vec4 cam_translate;
uniform mat4 cam_rotate;
uniform mat4 cam_persp;

void main()
{
	gl_Position = vec4(aPos.xyz, 1.0f);
	gl_Position = mdl_scale * gl_Position;
	gl_Position = mdl_rotate * gl_Position;
	gl_Position = gl_Position + mdl_translate + cam_translate;
	gl_Position = cam_rotate * gl_Position;
	gl_Position = cam_persp * gl_Position;
	vpos = aPos;
}