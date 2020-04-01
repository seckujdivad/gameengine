#pragma once

#include <wx/wxprec.h>
#include "../../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "../../render/ShaderProgram.h"
#include "Reflection.h"

class Material
{
private:
	glm::vec3 m_diffuse;
	glm::vec3 m_specular;
	float m_specular_highlight;

	Reflection* m_reflection;
	glm::vec3 m_reflection_intensity;

public:
	Material();
	~Material();

	void SetDiffuse(glm::vec3 diffuse);
	glm::vec3 GetDiffuse();
	void SetSpecular(glm::vec3 specular);
	glm::vec3 GetSpecular();
	void SetSpecularHighlight(float intensity);
	float GetSpecularHighlight();

	void RegisterUniforms(ShaderProgram* shader_program);
	void SetUniforms(ShaderProgram* shader_program);
};