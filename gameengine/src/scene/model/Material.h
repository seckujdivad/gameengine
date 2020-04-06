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
	glm::vec3 m_diffuse = glm::vec3(0.0f);
	glm::vec3 m_specular = glm::vec3(0.0f);
	float m_specular_highlight = 2.0f;

	Reflection* m_reflection;
	int m_reflection_mode = 0;

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
};