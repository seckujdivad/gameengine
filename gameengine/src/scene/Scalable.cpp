#include <wx/wxprec.h>
#include "Scalable.h"

bool Scalable::CheckIfRescaled(bool reset)
{
	if (this->m_rescaled)
	{
		this->m_rescaled = !reset;
		return true;
	}
	else
	{
		return false;
	}
}

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
	this->m_rescaled = true;
}

void Scalable::SetScale(std::array<GLfloat, 3> scale)
{
	this->m_scale = scale;
	this->m_rescaled = true;
}

void Scalable::SetScale(int index, GLfloat value)
{
	this->m_scale.at(index) = value;
	this->m_rescaled = true;
}

std::array<GLfloat, 3> Scalable::GetScale()
{
	return this->m_scale;
}

GLfloat Scalable::GetScale(int index)
{
	return this->m_scale.at(index);
}
