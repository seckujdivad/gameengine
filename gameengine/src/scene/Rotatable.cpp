#include <wx/wxprec.h>
#include "Rotatable.h"

bool Rotatable::CheckIfRotated(bool reset)
{
	if (this->m_rotated)
	{
		this->m_rotated = !reset;
		return true;
	}
	else
	{
		return false;
	}
}

Rotatable::Rotatable()
{
}

Rotatable::Rotatable(Rotatable& copyfrom)
{
	this->m_rotation = copyfrom.GetRotation();
}

Rotatable& Rotatable::operator=(Rotatable& copyfrom)
{
	this->m_rotation = copyfrom.GetRotation();

	return *this;
}

Rotatable::~Rotatable()
{
}

void Rotatable::SetRotation(GLfloat x, GLfloat y, GLfloat z)
{
	this->m_rotation = { x, y, z };
	this->m_rotated = true;
}

void Rotatable::SetRotation(std::array<GLfloat, 3> rotation)
{
	this->m_rotation = rotation;
	this->m_rotated = true;
}

void Rotatable::SetRotation(int index, GLfloat value)
{
	this->m_rotation.at(index) = value;
	this->m_rotated = true;
}

std::array<GLfloat, 3> Rotatable::GetRotation()
{
	return this->m_rotation;
}

GLfloat Rotatable::GetRotation(int index)
{
	return this->m_rotation.at(index);
}
