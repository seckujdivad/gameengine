#include "LocallyMovable.h"

#include <glm/ext.hpp>

LocallyMovable::LocallyMovable() : Positionable(), Rotatable()
{

}

void LocallyMovable::MoveLocally(double x, double y, double z)
{
	this->MoveLocally(glm::dvec3(x, y, z));
}

void LocallyMovable::MoveLocally(glm::dvec3 vec, bool inverse_rotation)
{
	glm::dmat4 rotation = inverse_rotation ? this->GetRotationMatrixInverse() : this->GetRotationMatrix();
	glm::dvec4 translation = glm::dvec4(vec, 0.0) * rotation;
	this->SetPosition(this->GetPosition() + glm::dvec3(translation));
}
