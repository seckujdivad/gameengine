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
	else if (index == 1)
	{
		this->m_scale.y = value;
	}
	else if (index == 2)
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

glm::dvec3 Scalable::GetScale() const
{
	return this->m_scale;
}

glm::dmat4 Scalable::GetScaleMatrix() const
{
	glm::dmat4 matrix = glm::dmat4(1.0);
	matrix = glm::scale(matrix, this->GetScale());
	return matrix;
}

glm::dmat4 Scalable::GetScaleMatrixInverse() const
{
	glm::dmat4 matrix = glm::dmat4(1.0);
	matrix = glm::scale(matrix, 1.0 / this->GetScale());
	return matrix;
}

double Scalable::GetScale(int index) const
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
