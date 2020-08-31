#include "OrientedBoundingBox.h"

OrientedBoundingBox::OrientedBoundingBox() : Nameable(), Positionable(), Rotatable(), Scalable()
{
}

bool OrientedBoundingBox::PointInBounds(glm::dvec3 point) const
{
	glm::vec3 obb_position = this->GetInverseRotationMatrix() * (point - this->GetPosition() + (0.5 * this->GetRotationMatrix() * this->GetDimensionsVec()));

	for (int i = 0; i < 3; i++)
	{
		if ((0.0 > obb_position[i]) || (this->GetDimensionsVec()[i] < obb_position[i]))
		{
			return false;
		}
	}
	return true;
}

bool OrientedBoundingBox::PointInBounds(double x, double y, double z) const
{
	return this->PointInBounds(glm::dvec3(x, y, z));
}

glm::dvec3 OrientedBoundingBox::GetDimensionsVec() const
{
	return this->GetScale() * 2.0;
}

glm::dmat3 OrientedBoundingBox::GetRotationMatrix() const
{
	glm::dmat4 matrix = glm::dmat4(1.0);
	matrix = glm::rotate(matrix, glm::radians(this->GetRotation(0)), glm::dvec3(1.0, 0.0f, 0.0));
	matrix = glm::rotate(matrix, glm::radians(this->GetRotation(1)), glm::dvec3(0.0, 1.0f, 0.0));
	matrix = glm::rotate(matrix, glm::radians(this->GetRotation(2)), glm::dvec3(0.0, 0.0f, 1.0));

	return glm::dmat3(matrix);
}

glm::dmat3 OrientedBoundingBox::GetInverseRotationMatrix() const
{
	glm::dmat4 matrix = glm::dmat4(1.0);
	matrix = glm::rotate(matrix, glm::radians(0.0 - this->GetRotation(0)), glm::dvec3(1.0, 0.0, 0.0));
	matrix = glm::rotate(matrix, glm::radians(0.0 - this->GetRotation(1)), glm::dvec3(0.0, 1.0, 0.0));
	matrix = glm::rotate(matrix, glm::radians(0.0 - this->GetRotation(2)), glm::dvec3(0.0, 0.0, 1.0));

	return glm::dmat3(matrix);
}

bool operator==(const OrientedBoundingBox& first, const OrientedBoundingBox& second)
{
	if (first.GetIdentifier() != second.GetIdentifier())
	{
		return false;
	}

	if (first.GetPosition() != second.GetPosition())
	{
		return false;
	}

	if (first.GetRotation() != second.GetRotation())
	{
		return false;
	}

	if (first.GetScale() != second.GetScale())
	{
		return false;
	}

	return true;
}

bool operator!=(const OrientedBoundingBox& first, const OrientedBoundingBox& second)
{
	return !(first == second);
}
