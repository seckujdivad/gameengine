#version 400 core
layout (location = 0) in vec3 inPos;
layout (location = 1) in vec3 inNormal;
layout (location = 2) in vec2 inUV;

out vec4 vertMdlSpacePos;
out vec4 vertSceneSpacePos;
out vec4 vertCamSpacePos;

out vec2 vertUV;

out vec4 vertMdlSpaceNormal;
out vec4 vertSceneSpaceNormal;

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

void main()
{
	//vertex transformations
	vertMdlSpacePos = vec4(inPos.xyz, 1.0f);

	// model
	vertSceneSpacePos = mdl_scale * vertMdlSpacePos;
	vertSceneSpacePos = mdl_rotate * vertSceneSpacePos;
	vertSceneSpacePos = vertSceneSpacePos + mdl_translate;
	vertSceneSpacePos = vertSceneSpacePos / vertSceneSpacePos.w;

	// camera
	vertCamSpacePos = vertSceneSpacePos + cam_translate;

	// perspective
	gl_Position = vertCamSpacePos;

	//outputs
	vertMdlSpaceNormal = normalize(vec4(inNormal.xyz, 1.0f));
	vertSceneSpaceNormal = mdl_rotate * vertMdlSpaceNormal;
	
	vertUV = inUV;
}