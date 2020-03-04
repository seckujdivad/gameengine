#pragma once

#include <wx/wxprec.h>
#include "GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "Entity.h"
#include "render/ShaderProgram.h"

class PointLight : public Entity
{
private:
	glm::vec3 m_intensity;

	int m_light_index;

public:
	PointLight(int light_index);
	~PointLight();

	void SetIntensity(glm::vec3 intensity);

	void RegisterUniforms(ShaderProgram* shader_program);
	void SetUniforms(ShaderProgram* shader_program);
};