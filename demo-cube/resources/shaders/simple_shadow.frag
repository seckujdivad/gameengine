#version 400 core
in vec4 FragPos;

uniform vec3 light_position;
uniform float light_far_plane;

void main()
{
	gl_FragDepth = length(FragPos.xyz - light_position) / light_far_plane;
}