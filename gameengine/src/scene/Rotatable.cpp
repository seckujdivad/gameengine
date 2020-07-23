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
	if (index == 1)
	{
		this->m_rotation.y = value;
	}
	if (index == 2)
	{
		this->m_rotation.z = value;
	}
	else
	{
		throw std::runtime_error("Index must be 0, 1 or 2");
	}

	this->m_rotated = true;
}

void Rotatable::SetRotation(glm::dvec3 rotation)
{
	this->m_rotation = rotation;
}

glm::dvec3 Rotatable::GetRotation()
{
	return this->m_rotation;
}

double Rotatable::GetRotation(int index)
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
