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

Scalable::Scalable(const Scalable& copyfrom)
{
	this->m_scale = copyfrom.m_scale;
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

void Scalable::SetScale(glm::vec3 scale)
{
	this->m_scale.at(0) = scale.x;
	this->m_scale.at(1) = scale.y;
	this->m_scale.at(2) = scale.z;
}

std::array<GLfloat, 3> Scalable::GetScale()
{
	return this->m_scale;
}

glm::vec3 Scalable::GetScaleVec()
{
	return glm::vec3(this->m_scale.at(0), this->m_scale.at(1), this->m_scale.at(2));
}

GLfloat Scalable::GetScale(int index)
{
	return this->m_scale.at(index);
}
