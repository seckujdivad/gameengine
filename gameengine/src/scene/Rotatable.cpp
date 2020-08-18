#include <wx/wxprec.h>
#include "Rotatable.h"

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

void Rotatable::SetRotation(double x, double y, double z)
{
	this->m_rotation = glm::dvec3(x, y, z);
	this->m_rotated = true;
}

void Rotatable::SetRotation(glm::dvec3 rotation)
{
	this->m_rotation = rotation;
	this->m_rotated = true;
}

void Rotatable::SetRotation(int index, double value)
{
	if (index == 0)
	{
		this->m_rotation.x = value;
	}
	else if (index == 1)
	{
		this->m_rotation.y = value;
	}
	else if (index == 2)
	{
		this->m_rotation.z = value;
	}
	else
	{
		throw std::runtime_error("Index must be 0, 1 or 2");
	}

	this->m_rotated = true;
}

glm::dvec3 Rotatable::GetRotation() const
{
	return this->m_rotation;
}

glm::dmat4 Rotatable::GetRotationMatrix() const
{
	glm::dmat4 matrix = glm::dmat4(1.0);
	matrix = glm::rotate(matrix, glm::radians(this->GetRotation(0)), glm::dvec3(1.0, 0.0, 0.0));
	matrix = glm::rotate(matrix, glm::radians(this->GetRotation(1)), glm::dvec3(0.0, 1.0, 0.0));
	matrix = glm::rotate(matrix, glm::radians(this->GetRotation(2)), glm::dvec3(0.0, 0.0, 1.0));
	return matrix;
}

glm::dmat4 Rotatable::GetRotationMatrixInverse() const
{
	glm::dmat4 matrix = glm::dmat4(1.0);
	matrix = glm::rotate(matrix, glm::radians(0 - this->GetRotation(0)), glm::dvec3(1.0, 0.0, 0.0));
	matrix = glm::rotate(matrix, glm::radians(0 - this->GetRotation(1)), glm::dvec3(0.0, 1.0, 0.0));
	matrix = glm::rotate(matrix, glm::radians(0 - this->GetRotation(2)), glm::dvec3(0.0, 0.0, 1.0));
	return matrix;
}

double Rotatable::GetRotation(int index) const
{
	if (index == 0)
	{
		return this->m_rotation.x;
	}
	if (index == 1)
	{
		return this->m_rotation.y;
	}
	if (index == 2)
	{
		return this->m_rotation.z;
	}
	else
	{
		throw std::runtime_error("Index must be 0, 1 or 2");
		return 0;
	}
}
