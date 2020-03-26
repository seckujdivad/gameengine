#include <wx/wxprec.h>
#include "Positionable.h"

Positionable::Positionable()
{
}

Positionable::Positionable(Positionable& copyfrom)
{
	this->m_position = copyfrom.GetPosition();
}

Positionable& Positionable::operator=(Positionable& copyfrom)
{
	this->m_position = copyfrom.GetPosition();

	return *this;
}

Positionable::~Positionable()
{
}

void Positionable::SetPosition(GLfloat x, GLfloat y, GLfloat z)
{
	this->m_position = { x, y, z };
}

void Positionable::SetPosition(std::array<GLfloat, 3> point)
{
	this->m_position = point;
}

void Positionable::SetPosition(int index, GLfloat value)
{
	this->m_position.at(index) = value;
}

std::array<GLfloat, 3> Positionable::GetPosition()
{
	return this->m_position;
}

GLfloat Positionable::GetPosition(int index)
{
	return this->m_position.at(index);
}
