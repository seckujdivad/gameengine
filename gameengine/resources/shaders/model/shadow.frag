#version 400 core
in vec4 geomCamSpacePos;

uniform float cam_clip_near;
uniform float cam_clip_far;

void main()
{
	gl_FragDepth = length(geomCamSpacePos.xyz) / (cam_clip_far - cam_clip_near);
}