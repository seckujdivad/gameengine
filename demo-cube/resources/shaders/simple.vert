#version 400 core
layout (location = 0) in vec3 inPos;
layout (location = 1) in vec3 inNormal;
layout (location = 2) in vec2 inUV;

out vec3 globalPos;
out vec3 globalNormal;
out vec2 globalUV;

uniform vec4 mdl_translate;
uniform mat4 mdl_rotate;
uniform mat4 mdl_scale;

uniform vec4 cam_translate;
uniform mat4 cam_rotate;
uniform mat4 cam_persp;

void main()
{
	gl_Position = vec4(inPos.xyz, 1.0f);
	gl_Position = mdl_scale * gl_Position;
	gl_Position = mdl_rotate * gl_Position;
	gl_Position = gl_Position + mdl_translate + cam_translate;
	gl_Position = cam_rotate * gl_Position;
	gl_Position = cam_persp * gl_Position;

	globalPos = inPos;
	globalNormal = inNormal;
	globalUV = inUV;
}