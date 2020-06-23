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

Positionable::Positionable(EventManager* evtman) : EventEmitter(evtman)
{
}

Positionable::Positionable(const Positionable& copyfrom) : EventEmitter(copyfrom)
{
	this->m_position = copyfrom.m_position;
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
	this->EmitEvent("repositioned", nlohmann::json::array({
		nlohmann::json::array({ this->m_position.at(0), this->m_position.at(1), this->m_position.at(2) }),
		nlohmann::json::array({ x, y, z })
		}));

	this->m_position = { x, y, z };
	this->m_repositioned = true;
}

void Positionable::SetPosition(std::array<GLfloat, 3> point)
{
	this->EmitEvent("repositioned", nlohmann::json::array({
		nlohmann::json::array({ this->m_position.at(0), this->m_position.at(1), this->m_position.at(2) }),
		nlohmann::json::array({ point.at(0), point.at(1), point.at(2) })
		}));

	this->m_position = point;
	this->m_repositioned = true;
}

void Positionable::SetPosition(int index, GLfloat value)
{
	std::array<GLfloat, 3> old_position = this->m_position;

	this->m_position.at(index) = value;
	this->m_repositioned = true;

	this->EmitEvent("repositioned", nlohmann::json::array({
		nlohmann::json::array({ old_position.at(0), old_position.at(1), old_position.at(2) }),
		nlohmann::json::array({ this->m_position.at(0), this->m_position.at(1), this->m_position.at(2) })
		}));
}

void Positionable::SetPosition(glm::vec3 position)
{
	this->EmitEvent("repositioned", nlohmann::json::array({
		nlohmann::json::array({ this->m_position.at(0), this->m_position.at(1), this->m_position.at(2) }),
		nlohmann::json::array({ position.x, position.y, position.z })
		}));

	this->m_position.at(0) = position.x;
	this->m_position.at(1) = position.y;
	this->m_position.at(2) = position.z;
}

std::array<GLfloat, 3> Positionable::GetPosition()
{
	return this->m_position;
}

GLfloat Positionable::GetPosition(int index)
{
	return this->m_position.at(index);
}

glm::vec3 Positionable::GetPositionVec()
{
	return glm::vec3(this->m_position.at(0), this->m_position.at(1), this->m_position.at(2));
}
