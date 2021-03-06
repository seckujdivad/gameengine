#include "PointLight.h"

PointLight::PointLight(RenderTextureReference reference) : Nameable(), Cubemap(reference)
{
	this->m_intensity = glm::vec3(0.0f);
}

void PointLight::SetIntensity(glm::vec3 intensity)
{
	this->m_intensity = intensity;
}

glm::vec3 PointLight::GetIntensity() const
{
	return this->m_intensity;
}

void PointLight::SetShadowBias(double bias)
{
	this->m_shadow_bias = bias;
}

double PointLight::GetShadowBias() const
{
	return this->m_shadow_bias;
}
