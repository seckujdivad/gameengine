#include <wx/wxprec.h>
#include "Scalable.h"

bool Scalable::CheckIfRescaled(bool reset)
{
	if (this->m_rescaled)
	{
		this->m_rescaled = !reset;
		return true;
	}
	else
	{
		return false;
	}
}

Scalable::Scalable()
{
}

void Scalable::SetScale(double x, double y, double z)
{
	this->m_scale = glm::dvec3(x, y, z);
	this->m_rescaled = true;
}

void Scalable::SetScale(int index, double value)
{
	if (index == 0)
	{
		this->m_scale.x = value;
	}
	if (index == 1)
	{
		this->m_scale.y = value;
	}
	if (index == 2)
	{
		this->m_scale.z = value;
	}
	else
	{
		throw std::runtime_error("Index must be 0, 1 or 2");
	}

	this->m_rescaled = true;
}

void Scalable::SetScale(glm::dvec3 scale)
{
	this->m_scale = scale;
}

glm::dvec3 Scalable::GetScale()
{
	return this->m_scale;
}

double Scalable::GetScale(int index)
{
	if (index == 0)
	{
		return this->m_scale.x;
	}
	if (index == 1)
	{
		return this->m_scale.y;
	}
	if (index == 2)
	{
		return this->m_scale.z;
	}
	else
	{
		throw std::runtime_error("Index must be 0, 1 or 2");
		return 0;
	}
}
