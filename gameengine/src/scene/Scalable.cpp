#include <wx/wxprec.h>
#include "Scalable.h"

Scalable::Scalable()
{
}

Scalable::Scalable(Scalable& copyfrom)
{
	this->m_scale = copyfrom.GetScale();
}

Scalable& Scalable::operator=(Scalable& copyfrom)
{
	this->m_scale = copyfrom.GetScale();
	
	return *this;
}

Scalable::~Scalable()
{
	
}

void Scalable::SetScale(GLfloat x, GLfloat y, GLfloat z)
{
	this->m_scale = { x, y, z };
}

void Scalable::SetScale(std::array<GLfloat, 3> scale)
{
	this->m_scale = scale;
}

void Scalable::SetScale(int index, GLfloat value)
{
	this->m_scale.at(index) = value;
}

std::array<GLfloat, 3> Scalable::GetScale()
{
	return this->m_scale;
}

GLfloat Scalable::GetScale(int index)
{
	return this->m_scale.at(index);
}
