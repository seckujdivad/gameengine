#include <wx/wxprec.h>
#include "Rotatable.h"

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
}

void Rotatable::SetRotation(std::array<GLfloat, 3> rotation)
{
	this->m_rotation = rotation;
}

void Rotatable::SetRotation(int index, GLfloat value)
{
	this->m_rotation.at(index) = value;
}

std::array<GLfloat, 3> Rotatable::GetRotation()
{
	return this->m_rotation;
}

GLfloat Rotatable::GetRotation(int index)
{
	return this->m_rotation.at(index);
}
