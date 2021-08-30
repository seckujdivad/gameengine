#include "Positionable.h"

#include <stdexcept>

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

void Positionable::SetPosition(glm::dvec3 position)
{
	this->m_position = position;
	this->m_repositioned = true;
}

glm::dvec3 Positionable::GetPosition() const
{
	return this->m_position;
}

glm::dmat4 Positionable::GetTranslationMatrix() const
{
	return ::GetTranslationMatrix(this->GetPosition());
}

glm::dmat4 Positionable::GetTranslationMatrixInverse() const
{
	return ::GetTranslationMatrix(0.0 - this->GetPosition());
}

glm::dmat4 GetTranslationMatrix(glm::dvec3 translation)
{
	glm::dmat4 matrix = glm::dmat4(1.0);
	matrix = glm::translate(matrix, translation);
	return matrix;
}
