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
	glm::vec3 m_reflection_intensity = glm::vec3(0.0f, 0.0f, 0.0f);

public:
	Material();
	~Material();

	void SetDiffuse(glm::vec3 diffuse);
	glm::vec3 GetDiffuse();
	void SetSpecular(glm::vec3 specular);
	glm::vec3 GetSpecular();
	void SetSpecularHighlight(float intensity);
	float GetSpecularHighlight();

	void SetReflection(Reflection* reflection);
	Reflection* GetReflection();
	void SetReflectionIntensity(glm::vec3 intensity);
	glm::vec3 GetReflectionIntensity();

	void RegisterUniforms(ShaderProgram* shader_program);
	void SetUniforms(ShaderProgram* shader_program);
};