#version 400 core
layout (location = 0) in vec3 inPos;
layout (location = 1) in vec3 inNormal;
layout (location = 2) in vec2 inUV;
layout (location = 3) in vec3 inTangent;
layout (location = 4) in vec3 inBitangent;

out vec4 globalMdlSpacePos;
out vec4 globalSceneSpacePos;
out vec4 globalCamSpacePos;

out vec2 globalUV;

out vec4 globalMdlSpaceNormal;
out vec4 globalSceneSpaceNormal;
out vec4 globalCamSpaceNormal;

out mat3 globalNormalTBN;

uniform vec4 mdl_translate;
uniform mat4 mdl_rotate;
uniform mat4 mdl_scale;

uniform vec4 cam_translate;
uniform mat4 cam_rotate;
uniform mat4 cam_persp;

void main()
{
	//vertex transformations
	globalMdlSpacePos = vec4(inPos.xyz, 1.0f);

	// model
	globalSceneSpacePos = mdl_scale * globalMdlSpacePos;
	globalSceneSpacePos = mdl_rotate * globalSceneSpacePos;
	globalSceneSpacePos = globalSceneSpacePos + mdl_translate;

	// camera
	globalCamSpacePos = globalSceneSpacePos + cam_translate;
	globalCamSpacePos = cam_rotate * globalCamSpacePos;

	// perspective
	gl_Position = cam_persp * globalCamSpacePos;

	//outputs
	globalMdlSpaceNormal = normalize(vec4(inNormal.xyz, 1.0f));
	globalSceneSpaceNormal = mdl_rotate * globalMdlSpaceNormal;
	globalCamSpaceNormal = cam_rotate * globalSceneSpaceNormal;
	
	globalUV = inUV;

	//create tbn matrix
	globalNormalTBN[0] = normalize(vec3(mdl_rotate * vec4(inTangent, 0.0f)));
	globalNormalTBN[1] = normalize(vec3(mdl_rotate * vec4(inBitangent, 0.0f)));
	globalNormalTBN[2] = globalSceneSpaceNormal.xyz;
}