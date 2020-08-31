#pragma once

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

	double GetRotation(int index) const;
	glm::dvec3 GetRotation() const;

	glm::dmat4 GetRotationMatrix() const;
	glm::dmat4 GetRotationMatrixInverse() const;

	bool CheckIfRotated(bool reset = true);
};