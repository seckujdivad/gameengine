#include "OrientedBoundingBox.h"

OrientedBoundingBox::OrientedBoundingBox() : Positionable(), Rotatable(), Scalable()
{
}

bool OrientedBoundingBox::PointInBounds(glm::vec3 point)
{
	glm::vec3 obb_position = this->GetInverseRotationMatrix() * (point - this->GetPositionVec() + (0.5f * this->GetRotationMatrix() * this->GetDimensionsVec()));

	for (int i = 0; i < 3; i++)
	{
		if ((0.0f > obb_position[i]) || (this->GetDimensionsVec()[i] < obb_position[i]))
		{
			return false;
		}
	}
	return true;
}

bool OrientedBoundingBox::PointInBounds(float x, float y, float z)
{
	return this->PointInBounds(glm::vec3(x, y, z));
}

glm::vec3 OrientedBoundingBox::GetDimensionsVec()
{
	return glm::vec3(this->GetScaleVec() * 2.0f);
}

glm::mat3 OrientedBoundingBox::GetRotationMatrix()
{
	if (this->CheckIfRotated(true))
	{
		glm::mat4 matrix = glm::mat4(1.0f);
		matrix = glm::rotate(matrix, glm::radians(this->GetRotation(0)), glm::vec3(1.0f, 0.0f, 0.0f));
		matrix = glm::rotate(matrix, glm::radians(this->GetRotation(1)), glm::vec3(0.0f, 1.0f, 0.0f));
		matrix = glm::rotate(matrix, glm::radians(this->GetRotation(2)), glm::vec3(0.0f, 0.0f, 1.0f));

		this->m_rotation_matrix = glm::mat3(matrix);

		glm::mat4 inverse_matrix = glm::mat4(1.0f);
		inverse_matrix = glm::rotate(inverse_matrix, glm::radians(0.0f - this->GetRotation(0)), glm::vec3(1.0f, 0.0f, 0.0f));
		inverse_matrix = glm::rotate(inverse_matrix, glm::radians(0.0f - this->GetRotation(1)), glm::vec3(0.0f, 1.0f, 0.0f));
		inverse_matrix = glm::rotate(inverse_matrix, glm::radians(0.0f - this->GetRotation(2)), glm::vec3(0.0f, 0.0f, 1.0f));

		this->m_rotation_inverse_matrix = glm::mat3(inverse_matrix);
	}
	
	return this->m_rotation_matrix;
}

glm::mat3 OrientedBoundingBox::GetInverseRotationMatrix()
{
	this->GetRotationMatrix();

	return this->m_rotation_inverse_matrix;
}
