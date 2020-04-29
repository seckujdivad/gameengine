#include <wx/wxprec.h>
#include "Positionable.h"

bool Positionable::CheckIfRepositioned(bool reset)
{
	if (this->m_repositioned)
	{
		this->m_repositioned = !reset;
		return true;
	}
	else
	{
		return false;
	}
}

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
	this->m_repositioned = true;
}

void Positionable::SetPosition(std::array<GLfloat, 3> point)
{
	this->m_position = point;
	this->m_repositioned = true;
}

void Positionable::SetPosition(int index, GLfloat value)
{
	this->m_position.at(index) = value;
	this->m_repositioned = true;
}

std::array<GLfloat, 3> Positionable::GetPosition()
{
	return this->m_position;
}

GLfloat Positionable::GetPosition(int index)
{
	return this->m_position.at(index);
}
