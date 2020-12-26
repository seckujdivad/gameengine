#version 400 core
layout (location = 0) in vec3 inPos;
layout (location = 1) in vec3 inNormal;
layout (location = 2) in vec2 inUV;

out vec3 vertMdlSpacePos;
out vec3 vertSceneSpacePos;
out vec3 vertCamSpacePos;

out vec2 vertUV;

out vec3 vertMdlSpaceNormal;
out vec3 vertSceneSpaceNormal;

uniform vec4 mdl_translate;
uniform mat4 mdl_rotate;
uniform mat4 mdl_scale;

uniform vec4 cam_translate;
uniform mat4 cam_rotate;
uniform mat4 cam_persp;
uniform float cam_clip_near;
uniform float cam_clip_far;
uniform mat4 cam_transform;
uniform mat4 cam_transform_inverse;

vec3 persp_div(vec4 vec)
{
	return vec.xyz / vec.w;
}

void main()
{
	//vertex transformations
	vertMdlSpacePos = inPos.xyz;

	// model
	vertSceneSpacePos = persp_div(mdl_scale * vec4(vertMdlSpacePos, 1.0f));
	vertSceneSpacePos = persp_div(mdl_rotate * vec4(vertSceneSpacePos, 1.0f));
	vertSceneSpacePos = vertSceneSpacePos + mdl_translate.xyz;

	// camera
	vertCamSpacePos = vertSceneSpacePos + cam_translate.xyz;

	//outputs
	vertMdlSpaceNormal = normalize(inNormal);
	vertSceneSpaceNormal = persp_div(mdl_rotate * vec4(vertMdlSpaceNormal, 1.0f));
	
	vertUV = inUV;
}