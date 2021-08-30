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

	void SetRotation(glm::dvec3 rotation);
	glm::dvec3 GetRotation() const;

	glm::dmat4 GetRotationMatrix() const;
	glm::dmat4 GetRotationMatrixInverse() const;

	bool CheckIfRotated(bool reset = true);
};

glm::dmat4 GetRotationMatrix(glm::dvec3 rotation);