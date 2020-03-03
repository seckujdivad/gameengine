#include <wx/wxprec.h>
#include "PointLight.h"

PointLight::PointLight()
{
	this->m_diffuse = glm::vec3(0.0f);
	this->m_specular = glm::vec3(0.0f);
}

PointLight::~PointLight()
{

}

void PointLight::SetDiffuse(glm::vec3 intensity)
{
	this->m_diffuse = intensity;
}

void PointLight::SetSpecular(glm::vec3 intensity)
{
	this->m_specular = intensity;
}