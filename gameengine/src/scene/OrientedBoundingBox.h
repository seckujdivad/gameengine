#pragma once

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "Positionable.h"
#include "Rotatable.h"
#include "Scalable.h"
#include "Nameable.h"

class OrientedBoundingBox : public Nameable, public Positionable, public Rotatable, public Scalable
{
public:
	OrientedBoundingBox();

	bool PointInBounds(glm::dvec3 point) const;
	bool PointInBounds(double x, double y, double z) const;

	glm::dvec3 GetDimensionsVec() const;
};

bool operator==(const OrientedBoundingBox& first, const OrientedBoundingBox& second);
bool operator!=(const OrientedBoundingBox& first, const OrientedBoundingBox& second);