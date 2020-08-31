#pragma once

#include <glm/glm.hpp>

#include "../Nameable.h"
#include "../Cubemap.h"

class PointLight : public Nameable, public Cubemap
{
private:
	glm::vec3 m_intensity;

	bool m_shadows_enabled = false;

	double m_shadow_bias = -0.05;

public:
	PointLight(RenderTextureReference reference);

	void SetIntensity(glm::vec3 intensity);
	glm::vec3 GetIntensity() const;

	void SetShadowsEnabled(bool enabled);
	bool GetShadowsEnabled() const;

	void SetShadowBias(double bias);
	double GetShadowBias() const;
};