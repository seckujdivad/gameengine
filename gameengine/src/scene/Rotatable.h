#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

class Rotatable
{
private:
	glm::dvec3 m_rotation = glm::dvec3(0.0);

	bool m_rotated = true;

public:
	Rotatable();

	void SetRotation(double x, double y, double z);
	void SetRotation(int index, double value);
	void SetRotation(glm::dvec3 scale);
	double GetRotation(int index);
	glm::dvec3 GetRotation();

	glm::dmat4 GetRotationMatrix();
	glm::dmat4 GetRotationMatrixInverse();

	bool CheckIfRotated(bool reset = true);
};