#version 400 core
layout (location = 0) in vec3 inPos;
layout (location = 1) in vec3 inNormal;
layout (location = 2) in vec2 inUV;

uniform vec4 mdl_translate;
uniform mat4 mdl_rotate;
uniform mat4 mdl_scale;


void main()
{
	//vertex transformations
	vec4 MdlSpacePos = vec4(inPos.xyz, 1.0f);

	// model
	vec4 SceneSpacePos = mdl_scale * MdlSpacePos;
	SceneSpacePos = mdl_rotate * SceneSpacePos;
	SceneSpacePos = SceneSpacePos + mdl_translate;

	//light
	gl_Position = SceneSpacePos;
}