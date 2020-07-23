#include <wx/wxprec.h>
#include "Positionable.h"

bool Positionable::CheckIfRepositioned(bool reset)
{
	if (this->m_repositioned)
	{
		this->m_repositioned = !reset;
		return true;
	}
	else
	{
		return false;
	}
}

Positionable::Positionable()
{
}

void Positionable::SetPosition(double x, double y, double z)
{
	this->m_position = { x, y, z };
	this->m_repositioned = true;
}

void Positionable::SetPosition(glm::dvec3 point)
{
	this->m_position = point;
	this->m_repositioned = true;
}

void Positionable::SetPosition(int index, double value)
{
	if (index == 0)
	{
		this->m_position.x = value;
	}
	if (index == 1)
	{
		this->m_position.y = value;
	}
	if (index == 2)
	{
		this->m_position.z = value;
	}
	else
	{
		throw std::runtime_error("Index must be 0, 1 or 2");
	}

	this->m_repositioned = true;
}

glm::dvec3 Positionable::GetPosition()
{
	return this->m_position;
}

double Positionable::GetPosition(int index)
{
	if (index == 0)
	{
		return this->m_position.x;
	}
	if (index == 1)
	{
		return this->m_position.y;
	}
	if (index == 2)
	{
		return this->m_position.z;
	}
	else
	{
		throw std::runtime_error("Index must be 0, 1 or 2");
		return 0;
	}
}
