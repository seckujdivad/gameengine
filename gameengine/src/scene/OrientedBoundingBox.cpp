#include "OrientedBoundingBox.h"

OrientedBoundingBox::OrientedBoundingBox() : Nameable(), Positionable(), Rotatable(), Scalable()
{
}

bool OrientedBoundingBox::PointInBounds(glm::dvec3 point) const
{
	glm::vec3 obb_position = glm::dmat3(this->GetRotationMatrixInverse()) * (point - this->GetPosition() + (0.5 * glm::dmat3(this->GetRotationMatrix()) * this->GetDimensionsVec()));

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
