#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

class Scalable
{
private:
	glm::dvec3 m_scale = glm::dvec3(0.0);

	bool m_rescaled = true;
	
public:
	Scalable();

	void SetScale(double x, double y, double z);
	void SetScale(int index, double value);
	void SetScale(glm::dvec3 scale);
	double GetScale(int index);
	glm::dvec3 GetScale();

	glm::dmat4 GetScaleMatrix();
	glm::dmat4 GetScaleMatrixInverse();

	bool CheckIfRescaled(bool reset = true);
};