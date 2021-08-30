#include "Rotatable.h"

#include <stdexcept>

bool Rotatable::CheckIfRotated(bool reset)
{
	if (this->m_rotated)
	{
		this->m_rotated = !reset;
		return true;
	}
	else
	{
		return false;
	}
}

Rotatable::Rotatable()
{
}

void Rotatable::SetRotation(glm::dvec3 rotation)
{
	this->m_rotation = rotation;
	this->m_rotated = true;
}

glm::dvec3 Rotatable::GetRotation() const
{
	return this->m_rotation;
}

glm::dmat4 Rotatable::GetRotationMatrix() const
{
	return ::GetRotationMatrix(this->GetRotation());
}

glm::dmat4 Rotatable::GetRotationMatrixInverse() const
{
	return ::GetRotationMatrix(0.0 - this->GetRotation());
}

glm::dmat4 GetRotationMatrix(glm::dvec3 rotation)
{
	glm::dmat4 matrix = glm::dmat4(1.0);
	matrix = glm::rotate(matrix, glm::radians(rotation.x), glm::dvec3(1.0, 0.0, 0.0));
	matrix = glm::rotate(matrix, glm::radians(rotation.y), glm::dvec3(0.0, 1.0, 0.0));
	matrix = glm::rotate(matrix, glm::radians(rotation.z), glm::dvec3(0.0, 0.0, 1.0));
	return matrix;
}
