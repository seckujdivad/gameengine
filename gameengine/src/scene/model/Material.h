#pragma once

#include <wx/wxprec.h>
#include "../../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "../../render/ShaderProgram.h"
#include "Reflection.h"

struct MaterialSSRConfig
{
	int resolution = 1;
	float cast_distance_limit = 1.0f;
	float depth_acceptance = 0.1f;
	float max_cam_distance = 10.0f;
	bool appear_in_ssr = false;
};

class Material
{
private:
	glm::vec3 m_diffuse = glm::vec3(0.0f);
	glm::vec3 m_specular = glm::vec3(0.0f);
	float m_specular_highlight = 2.0f;

	Reflection* m_reflection = nullptr;
	int m_reflection_mode = 0;

	bool m_ssr_enabled = false;
	MaterialSSRConfig m_ssr_config;

public:
	Material();
	~Material();

	void SetDiffuse(glm::vec3 diffuse);
	glm::vec3 GetDiffuse();
	void SetSpecular(glm::vec3 specular);
	glm::vec3 GetSpecular();
	void SetSpecularHighlight(float intensity);
	float GetSpecularHighlight();

	void SetReflection(Reflection* reflection, int mode);
	Reflection* GetReflection();

	void RegisterUniforms(ShaderProgram* shader_program);
	void SetUniforms(ShaderProgram* shader_program);

	void EnableSSR(bool enable);
	bool SSREnabled();
	void SetSSRConfig(MaterialSSRConfig config);
	MaterialSSRConfig GetSSRConfig();
};