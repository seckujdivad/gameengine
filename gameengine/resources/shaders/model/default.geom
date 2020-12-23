#version 430 core
layout (triangles) in;
layout (triangle_strip, max_vertices = 18) out;

in vec4 teseMdlSpacePos[];
in vec4 teseSceneSpacePos[];
in vec4 teseCamSpacePos[];

in vec2 teseUV[];

in vec4 teseMdlSpaceNormal[];
in vec4 teseSceneSpaceNormal[];

out vec4 geomMdlSpacePos;
out vec4 geomSceneSpacePos;
out vec4 geomCamSpacePos;
out vec3 geomTangentSpacePos;

out vec2 geomUV;

out vec4 geomMdlSpaceNormal;
out vec4 geomSceneSpaceNormal;
out vec4 geomCamSpaceNormal;

out mat3 geomNormalTBN;

out vec3 geomTangentSpaceCameraPos;

uniform mat4 cubemap_transform[6];
uniform bool is_cubemap;

uniform vec4 cam_translate;
uniform mat4 cam_rotate;
uniform mat4 cam_persp;

uniform mat4 mdl_rotate;

void set_outputs(int index)
{
	//select correct values for the index
	geomMdlSpacePos = teseMdlSpacePos[index % 3];
	geomSceneSpacePos = teseSceneSpacePos[index % 3];
	geomCamSpacePos = teseCamSpacePos[index % 3];

	geomUV = teseUV[index % 3];

	geomMdlSpaceNormal = teseMdlSpaceNormal[index % 3];
	geomSceneSpaceNormal = teseSceneSpaceNormal[index % 3];

	//calculate post camera transformation values (which are affected by the cubemap face)
	geomCamSpacePos = (cubemap_transform[gl_Layer] * cam_rotate * geomCamSpacePos) / geomCamSpacePos.w;

	gl_Position = cam_persp * geomCamSpacePos;

	geomCamSpaceNormal = cubemap_transform[gl_Layer] * cam_rotate * teseSceneSpaceNormal[index % 3];

	//calculate per-vertex values for the index
	// calculate tangent and bitangent
	const vec3 edge1 = teseMdlSpacePos[1].xyz - teseMdlSpacePos[0].xyz;
	const vec3 edge2 = teseMdlSpacePos[2].xyz - teseMdlSpacePos[0].xyz;

	const vec2 edgeuv1 = teseUV[1] - teseUV[0];
	const vec2 edgeuv2 = teseUV[2] - teseUV[0];

	//TODO: test as directional vec4s
	vec3 tangent = vec3(
		(edgeuv2.y * edge1.x) - (edgeuv1.y * edge2.x),
		(edgeuv2.y * edge1.y) - (edgeuv1.y * edge2.y),
		(edgeuv2.y * edge1.z) - (edgeuv1.y * edge2.z)
	);

	vec3 bitangent = vec3(
		(edgeuv1.x * edge2.x) - (edgeuv2.x * edge1.x),
		(edgeuv1.x * edge2.y) - (edgeuv2.x * edge1.y),
		(edgeuv1.x * edge2.z) - (edgeuv2.x * edge1.z)
	);

	geomNormalTBN[0] = normalize(vec3(mdl_rotate * vec4(tangent, 1.0f)));
	geomNormalTBN[1] = normalize(vec3(mdl_rotate * vec4(bitangent, 1.0f)));
	geomNormalTBN[2] = geomSceneSpaceNormal.xyz;

	geomTangentSpacePos = geomNormalTBN * geomSceneSpacePos.xyz;
	geomTangentSpaceCameraPos = geomNormalTBN * (0 - cam_translate.xyz);
}

void main()
{
	int num_layers = is_cubemap ? 6 : 1;

	for (int layer = 0; layer < num_layers; layer++)
	{
		for (int i = 0; i < 3; i++)
		{
			gl_Layer = layer;
			set_outputs(i);
			EmitVertex();
		}
		EndPrimitive();
	}
}