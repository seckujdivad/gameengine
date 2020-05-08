#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "Positionable.h"
#include "Rotatable.h"
#include "Scalable.h"

class OrientedBoundingBox : public Positionable, public Rotatable, public Scalable
{
private:
	glm::mat3 m_rotation_matrix = glm::mat3(1.0f);
	glm::mat3 m_rotation_inverse_matrix = glm::mat3(1.0f);

public:
	OrientedBoundingBox();

	bool PointInBounds(glm::vec3 point);
	bool PointInBounds(float x, float y, float z);

	glm::vec3 GetDimensionsVec();
	glm::mat3 GetRotationMatrix();
	glm::mat3 GetInverseRotationMatrix();
};