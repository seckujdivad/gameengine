#include <wx/wxprec.h>
#include "Entity.h"

Entity::Entity() : Nameable(), Positionable()
{
	this->m_rotation = { 0.0f, 0.0f, 0.0f };
	this->m_scale = { 0.0f, 0.0f, 0.0f };
}

Entity::Entity(Entity& copyfrom) : Nameable(copyfrom), Positionable(copyfrom)
{
	this->m_rotation = copyfrom.GetRotation();
	this->m_scale = copyfrom.GetScale();
}

Entity& Entity::operator=(Entity& copyfrom)
{
	Nameable::operator=(copyfrom);
	Positionable::operator=(copyfrom);

	return *this;
}

Entity::~Entity()
{

}

void Entity::SetRotation(GLfloat x, GLfloat y, GLfloat z)
{
	this->m_rotation = { x, y, z };
}

void Entity::SetRotation(std::array<GLfloat, 3> rotation)
{
	this->m_rotation = rotation;
}

void Entity::SetRotation(int index, GLfloat value)
{
	this->m_rotation.at(index) = value;
}

std::array<GLfloat, 3> Entity::GetRotation()
{
	return this->m_rotation;
}

GLfloat Entity::GetRotation(int index)
{
	return this->m_rotation.at(index);
}

void Entity::SetScale(GLfloat x, GLfloat y, GLfloat z)
{
	this->m_scale = { x, y, z };
}

void Entity::SetScale(std::array<GLfloat, 3> scale)
{
	this->m_scale = scale;
}

void Entity::SetScale(int index, GLfloat value)
{
	this->m_scale.at(index) = value;
}

std::array<GLfloat, 3> Entity::GetScale()
{
	return this->m_scale;
}

GLfloat Entity::GetScale(int index)
{
	return this->m_scale.at(index);
}