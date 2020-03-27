#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "Positionable.h"
#include "Rotatable.h"

class LocallyMovable : public Positionable, public Rotatable
{
private:
public:
	LocallyMovable();

	void MoveLocally(float x, float y, float z);
};