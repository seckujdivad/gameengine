#pragma once

#include <glm/glm.hpp>

#include "Positionable.h"
#include "Rotatable.h"

class LocallyMovable : public Positionable, public Rotatable
{
private:
public:
	LocallyMovable();

	void MoveLocally(double x, double y, double z);
	void MoveLocally(glm::dvec3 vec);
};