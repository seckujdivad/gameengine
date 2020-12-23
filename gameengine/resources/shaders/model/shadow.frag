#version 400 core
in vec3 geomCamSpacePos;

uniform float cam_clip_near;
uniform float cam_clip_far;

void main()
{
	gl_FragDepth = length(geomCamSpacePos) / (cam_clip_far - cam_clip_near);
}