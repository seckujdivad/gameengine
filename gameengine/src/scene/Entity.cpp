#include <wx/wxprec.h>
#include "Entity.h"

Entity::Entity()
{
	this->m_position = { 0.0f, 0.0f, 0.0f };
	this->m_rotation = { 0.0f, 0.0f, 0.0f };
	this->m_scale = { 0.0f, 0.0f, 0.0f };

	this->m_identifier = "";
}

Entity::Entity(Entity& copyfrom)
{
	this->m_position = copyfrom.GetPosition();
	this->m_rotation = copyfrom.GetRotation();
	this->m_scale = copyfrom.GetScale();

	this->m_identifier = copyfrom.GetIdentifier();
}

Entity::~Entity()
{

}

void Entity::SetIdentifier(std::string identifier)
{
	this->m_identifier = identifier;
}

std::string Entity::GetIdentifier()
{
	return this->m_identifier;
}

void Entity::SetPosition(GLfloat x, GLfloat y, GLfloat z)
{
	this->m_position = { x, y, z };
}

void Entity::SetPosition(std::array<GLfloat, 3> point)
{
	this->m_position = point;
}

void Entity::SetPosition(int index, GLfloat value)
{
	this->m_position.at(index) = value;
}

std::array<GLfloat, 3> Entity::GetPosition()
{
	return this->m_position;
}

GLfloat Entity::GetPosition(int index)
{
	return this->m_position.at(index);
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