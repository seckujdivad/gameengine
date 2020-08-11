#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "Positionable.h"
#include "Rotatable.h"
#include "Scalable.h"
#include "Nameable.h"

class OrientedBoundingBox : public Nameable, public Positionable, public Rotatable, public Scalable
{
private:
	glm::dmat3 m_rotation_matrix = glm::dmat3(1.0);
	glm::dmat3 m_rotation_inverse_matrix = glm::dmat3(1.0);

public:
	OrientedBoundingBox();

	bool PointInBounds(glm::dvec3 point);
	bool PointInBounds(double x, double y, double z);

	glm::dvec3 GetDimensionsVec();
	glm::dmat3 GetRotationMatrix();
	glm::dmat3 GetInverseRotationMatrix();
};
