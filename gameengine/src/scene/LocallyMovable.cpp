#include <wx/wxprec.h>
#include "LocallyMovable.h"

LocallyMovable::LocallyMovable() : Positionable(), Rotatable()
{

}

void LocallyMovable::MoveLocally(double x, double y, double z)
{
	this->MoveLocally(glm::dvec3(x, y, z));
}

void LocallyMovable::MoveLocally(glm::dvec3 vec)
{
	glm::dvec4 translation = glm::dvec4(0.0 - vec, 0.0);

	glm::dmat4 rotation = glm::dmat4(1.0);
	glm::dvec3 axis;
	for (int i = 2; i > -1; i--)
	{
		axis = glm::dvec4(0.0);
		axis[i] = 1.0;
		rotation = glm::rotate(rotation, glm::radians(this->GetRotation(i)), axis);
	}

	translation = rotation * translation;

	glm::dvec3 final_translation = glm::dvec3(translation);

	for (int i = 0; i < 3; i++)
	{
		this->SetPosition(i, this->GetPosition(i) + final_translation[i]);
	}
}
