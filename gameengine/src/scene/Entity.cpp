#include <wx/wxprec.h>
#include "Entity.h"

Entity::Entity() : Nameable(), Positionable(), Rotatable(), Scalable()
{
	
}

Entity::Entity(Entity& copyfrom) : Nameable(copyfrom), Positionable(copyfrom), Rotatable(copyfrom), Scalable(copyfrom)
{
	
}

Entity& Entity::operator=(Entity& copyfrom)
{
	Nameable::operator=(copyfrom);
	Positionable::operator=(copyfrom);
	Rotatable::operator=(copyfrom);
	Scalable::operator=(copyfrom);

	return *this;
}

Entity::~Entity()
{

}