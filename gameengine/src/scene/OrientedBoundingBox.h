#pragma once

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

	bool PointInBounds(glm::dvec3 point) const;
	bool PointInBounds(double x, double y, double z) const;

	glm::dvec3 GetDimensionsVec() const;
	glm::dmat3 GetRotationMatrix() const;
	glm::dmat3 GetInverseRotationMatrix() const;
};

bool operator==(const OrientedBoundingBox& first, const OrientedBoundingBox& second);
bool operator!=(const OrientedBoundingBox& first, const OrientedBoundingBox& second);