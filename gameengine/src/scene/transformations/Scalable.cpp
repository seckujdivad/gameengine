#include "Scalable.h"

#include <stdexcept>

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
	return ::GetScaleMatrix(this->GetScale());
}

glm::dmat4 Scalable::GetScaleMatrixInverse() const
{
	return ::GetScaleMatrix(1.0 / this->GetScale());
}

glm::dmat4 GetScaleMatrix(glm::dvec3 scale)
{
	glm::dmat4 matrix = glm::dmat4(1.0);
	matrix = glm::scale(matrix, scale);
	return matrix;
}
