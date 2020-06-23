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

Scalable::Scalable(EventManager* evtman) : EventEmitter(evtman)
{
}

Scalable::Scalable(const Scalable& copyfrom) : EventEmitter(copyfrom)
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
	this->EmitEvent("scaled", nlohmann::json::array({
		nlohmann::json::array({ this->m_scale.at(0), this->m_scale.at(1), this->m_scale.at(2) }),
		nlohmann::json::array({ x, y, z })
		}));

	this->m_scale = { x, y, z };
	this->m_rescaled = true;
}

void Scalable::SetScale(std::array<GLfloat, 3> scale)
{
	this->EmitEvent("scaled", nlohmann::json::array({
		nlohmann::json::array({ this->m_scale.at(0), this->m_scale.at(1), this->m_scale.at(2) }),
		nlohmann::json::array({ scale.at(0), scale.at(1), scale.at(2) })
		}));

	this->m_scale = scale;
	this->m_rescaled = true;
}

void Scalable::SetScale(int index, GLfloat value)
{
	std::array<GLfloat, 3> old_scale = this->m_scale;

	this->m_scale.at(index) = value;
	this->m_rescaled = true;

	this->EmitEvent("scaled", nlohmann::json::array({
		nlohmann::json::array({ old_scale.at(0), old_scale.at(1), old_scale.at(2) }),
		nlohmann::json::array({ this->m_scale.at(0), this->m_scale.at(1), this->m_scale.at(2) })
		}));
}

void Scalable::SetScale(glm::vec3 scale)
{
	this->EmitEvent("rotated", nlohmann::json::array({
		nlohmann::json::array({ this->m_scale.at(0), this->m_scale.at(1), this->m_scale.at(2) }),
		nlohmann::json::array({ scale.x, scale.y, scale.z })
		}));

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
