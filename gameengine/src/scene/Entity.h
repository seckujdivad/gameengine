#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include "Positionable.h"
#include "Rotatable.h"
#include "Scalable.h"
#include "Nameable.h"

#include <array>
#include <string>


class Entity : public Nameable, public Positionable, public Rotatable, public Scalable
{
private:
	
public:
	Entity();
	Entity(Entity& copyfrom);
	Entity& operator=(Entity& copyfrom);
	~Entity();
};