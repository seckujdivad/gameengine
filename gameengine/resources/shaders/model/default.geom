#version 430 core
layout (triangles) in;
layout (triangle_strip, max_vertices = 18) out;

in vec3 teseMdlSpacePos[];
in vec3 teseSceneSpacePos[];
in vec3 teseCamSpacePos[];

in vec2 teseUV[];

in vec3 teseMdlSpaceNormal[];
in vec3 teseSceneSpaceNormal[];

out vec3 geomMdlSpacePos;
out vec3 geomSceneSpacePos;
out vec3 geomCamSpacePos;
out vec3 geomTangentSpacePos;

out vec2 geomUV;

out vec3 geomMdlSpaceNormal;
out vec3 geomSceneSpaceNormal;
out vec3 geomCamSpaceNormal;

out mat3 geomNormalTBN;

out vec3 geomTangentSpaceCameraPos;

uniform mat4 cubemap_transform[6];
uniform bool is_cubemap;

uniform vec4 cam_translate;
uniform mat4 cam_rotate;
uniform mat4 cam_persp;

uniform mat4 mdl_rotate;

const int NUM_VERTICES = 3;

vec3 persp_div(vec4 vec)
{
	return vec.xyz / vec.w;
}

void set_outputs(int index)
{
	//select correct values for the index
	geomMdlSpacePos = teseMdlSpacePos[index % NUM_VERTICES];
	geomSceneSpacePos = teseSceneSpacePos[index % NUM_VERTICES];
	geomCamSpacePos = teseCamSpacePos[index % NUM_VERTICES];

	geomUV = teseUV[index % NUM_VERTICES];

	geomMdlSpaceNormal = teseMdlSpaceNormal[index % NUM_VERTICES];
	geomSceneSpaceNormal = teseSceneSpaceNormal[index % NUM_VERTICES];

	//calculate post camera transformation values (which are affected by the cubemap face)
	geomCamSpacePos = persp_div(cubemap_transform[gl_Layer] * cam_rotate * vec4(geomCamSpacePos, 1.0f));

	gl_Position = cam_persp * vec4(geomCamSpacePos, 1.0f);

	geomCamSpaceNormal = persp_div(cubemap_transform[gl_Layer] * cam_rotate * vec4(teseSceneSpaceNormal[index % NUM_VERTICES], 0.0f));

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

	geomNormalTBN[0] = normalize(persp_div(mdl_rotate * vec4(tangent, 1.0f)));
	geomNormalTBN[1] = normalize(persp_div(mdl_rotate * vec4(bitangent, 1.0f)));
	geomNormalTBN[2] = geomSceneSpaceNormal;

	geomTangentSpacePos = geomNormalTBN * geomSceneSpacePos;
	geomTangentSpaceCameraPos = geomNormalTBN * (0.0f - cam_translate.xyz);
}

void main()
{
	int num_layers = is_cubemap ? 6 : 1;

	for (int layer = 0; layer < num_layers; layer++)
	{
		gl_Layer = layer;

		vec3 ccw_normal = cross(teseSceneSpacePos[1] - teseSceneSpacePos[0], teseSceneSpacePos[2] - teseSceneSpacePos[0]);
		bool flip_winding = length(normalize(ccw_normal) + normalize(teseSceneSpaceNormal[0])) < 1.0f;

		for (int i = 0; i < NUM_VERTICES; i++)
		{
			if (flip_winding)
			{
				set_outputs(- 1 - i);
			}
			else
			{
				set_outputs(i);
			}

			EmitVertex();
		}
		EndPrimitive();
	}
}