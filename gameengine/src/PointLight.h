#pragma once

#include <wx/wxprec.h>
#include "GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "Entity.h"

class PointLight : public Entity
{
private:
	glm::vec3 m_diffuse;
	glm::vec3 m_specular;

public:
	PointLight();
	~PointLight();

	void SetDiffuse(glm::vec3 intensity);
	void SetSpecular(glm::vec3 intensity);
};