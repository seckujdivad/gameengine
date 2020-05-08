#pragma once

#include <wx/wxprec.h>
#include "../../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "../../render/ShaderProgram.h"
#include "Reflection.h"

struct MaterialSSRConfig
{
	float resolution = 1.0f;
	float cast_distance_limit = 1.0f;
	float depth_acceptance = 0.1f;
	float max_cam_distance = 10.0f;
	bool appear_in_ssr = false;
	int refinements = 1;
};

class Material
{
private:
	glm::vec3 m_diffuse = glm::vec3(0.0f);
	glm::vec3 m_specular = glm::vec3(0.0f);
	float m_specular_highlight = 2.0f;

	bool m_ssr_enabled = false;
	MaterialSSRConfig m_ssr_config;

	std::vector<Reflection*> m_reflections;
	std::vector<int> m_reflection_modes;

public:
	Material();
	~Material();

	void SetDiffuse(glm::vec3 diffuse);
	glm::vec3 GetDiffuse();
	void SetSpecular(glm::vec3 specular);
	glm::vec3 GetSpecular();
	void SetSpecularHighlight(float intensity);
	float GetSpecularHighlight();

	void AddReflection(Reflection* reflection, int mode);
	std::vector<Reflection*> GetReflections();

	void RegisterUniforms(ShaderProgram* shader_program);
	void SetUniforms(ShaderProgram* shader_program);

	void EnableSSR(bool enable);
	bool SSREnabled();
	void SetSSRConfig(MaterialSSRConfig config);
	MaterialSSRConfig GetSSRConfig();
};