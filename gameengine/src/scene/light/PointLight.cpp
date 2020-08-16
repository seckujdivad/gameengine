#include "PointLight.h"

PointLight::PointLight(RenderTextureReference reference) : Nameable(), Cubemap(reference)
{
	this->m_intensity = glm::vec3(0.0f);
}

void PointLight::SetIntensity(glm::vec3 intensity)
{
	this->m_intensity = intensity;
}

glm::vec3 PointLight::GetIntensity()
{
	return this->m_intensity;
}

void PointLight::SetShadowsEnabled(bool enabled)
{
	this->m_shadows_enabled = enabled;
}

bool PointLight::GetShadowsEnabled()
{
	return this->m_shadows_enabled;
}

void PointLight::SetShadowBias(double bias)
{
	this->m_shadow_bias = bias;
}

double PointLight::GetShadowBias()
{
	return this->m_shadow_bias;
}
