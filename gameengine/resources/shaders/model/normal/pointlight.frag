#version 430 core

#if !defined(LIGHT_ATTENUATION_A)
#define LIGHT_ATTENUATION_A 0.032f
#endif

#if !defined(LIGHT_ATTENUATION_B)
#define LIGHT_ATTENUATION_B 0.09f
#endif

#if !defined(LIGHT_ATTENUATION_C)
#define LIGHT_ATTENUATION_C 1.0f
#endif

#if defined(ACCESS_CUBE_MAPS)
#define INPUT_TEXTURE sampler2DArray
#else
#define INPUT_TEXTURE sampler2D
#endif

layout(location = 0) out vec4 data_out;

in vec3 geomMdlSpacePos;
in vec3 geomSceneSpacePos;
in vec3 geomCamSpacePos;
in vec3 geomTangentSpacePos;

in vec2 geomUV;

in vec3 geomMdlSpaceNormal;
in vec3 geomSceneSpaceNormal;
in vec3 geomCamSpaceNormal;

in mat3 geomNormalTBN;

in vec3 geomTangentSpaceCameraPos;

//gbuffers
uniform INPUT_TEXTURE gbufferDepth;
uniform INPUT_TEXTURE gbufferNormal;
uniform INPUT_TEXTURE gbufferSpecular;
uniform INPUT_TEXTURE gbufferDiffuse;

//light attributes
uniform vec3 light_position;
uniform vec3 light_intensity;
uniform samplerCube light_cubemap;
uniform bool light_draw_shadows;
uniform float light_far_plane;
uniform float light_bias;

uniform vec4 cam_translate;
uniform mat4 cam_transform_inverse;
uniform mat4 cam_persp_inverse;

uniform mat4 cubemap_transform_inverse[6];

uniform vec2 screen_dimensions;

vec3 PerspDiv(vec4 vec)
{
	return vec.xyz / vec.w;
}

vec4 GetTexture(INPUT_TEXTURE tex, vec2 texel)
{
#if defined(ACCESS_CUBE_MAPS)
	return texture(tex, vec3(texel, gl_Layer));
#else
	return texture(tex, texel);
#endif
}

vec4 GetTexture(INPUT_TEXTURE tex)
{
	return GetTexture(tex, gl_FragCoord.xy / screen_dimensions);
}

void main()
{
	vec2 gbuffer_sample = gl_FragCoord.xy / screen_dimensions;

	vec3 scene_space_normal = normalize((GetTexture(gbufferNormal).xyz * 2.0f) - 1.0f);

	vec3 screen_space_pos = (vec3(gbuffer_sample, GetTexture(gbufferDepth).r) * 2.0f) - 1.0f;
	vec3 scene_space_pos = PerspDiv(cam_transform_inverse * vec4(screen_space_pos, 1.0f));

	vec3 frag_to_light = normalize(light_position - scene_space_pos);
	vec3 frag_to_cam = normalize(0 - cam_translate.xyz - scene_space_pos);

	//diffuse
	float diffuse_intensity = min(max(dot(scene_space_normal, frag_to_light), 0.0f), 1.0f);
	vec3 intensity = diffuse_intensity * light_intensity * GetTexture(gbufferDiffuse).rgb;

	//specular
	vec3 halfway_dir = normalize(frag_to_cam + frag_to_light);
	float specular_highlight = GetTexture(gbufferNormal).a * 100.0f;
	float specular_intensity = min(pow(max(dot(scene_space_normal, halfway_dir), 0.0f), specular_highlight), 1.0f);
	intensity += float(diffuse_intensity > 0.0f) * specular_intensity * light_intensity * GetTexture(gbufferSpecular).rgb;

	//shadow
	float depth_sample = texture(light_cubemap, 0.0f - frag_to_light).r;
	float corrected_depth_sample = depth_sample * light_far_plane;

	float frag_depth = length(light_position - scene_space_pos);

	bool frag_in_range = depth_sample < 1.0f;
	bool frag_obscured = (frag_depth - light_bias) > corrected_depth_sample;
	bool frag_in_shadow = frag_in_range && frag_obscured;
	intensity *= light_draw_shadows ? float(!frag_in_shadow) : 1.0f;

	float attenuation_distance = frag_depth + length(0 - cam_translate.xyz - scene_space_pos);
	intensity /= (LIGHT_ATTENUATION_A * pow(attenuation_distance, 2.0f)) + (LIGHT_ATTENUATION_B * attenuation_distance) + LIGHT_ATTENUATION_C;

	data_out = vec4(intensity, 1.0f);
}