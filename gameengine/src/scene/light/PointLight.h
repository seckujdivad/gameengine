#pragma once

#include <wx/wxprec.h>
#include "../../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "../Nameable.h"
#include "../Cubemap.h"

class PointLight : public Nameable, public Cubemap
{
private:
	glm::vec3 m_intensity;

	bool m_shadows_enabled = false;

	double m_shadow_bias = -0.05f;

public:
	PointLight(CubemapReference reference);

	void SetIntensity(glm::vec3 intensity);
	glm::vec3 GetIntensity();

	void SetShadowsEnabled(bool enabled);
	bool GetShadowsEnabled();

	void SetShadowBias(double bias);
	double GetShadowBias();
};