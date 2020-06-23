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

Rotatable::Rotatable(EventManager* evtman) : EventEmitter(evtman)
{
}

Rotatable::Rotatable(const Rotatable& copyfrom) : EventEmitter(copyfrom)
{
	this->m_rotation = copyfrom.m_rotation;
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
	this->EmitEvent("rotated", nlohmann::json::array({
		nlohmann::json::array({ this->m_rotation.at(0), this->m_rotation.at(1), this->m_rotation.at(2) }),
		nlohmann::json::array({ x, y, z })
		}));

	this->m_rotation = { x, y, z };
	this->m_rotated = true;
}

void Rotatable::SetRotation(std::array<GLfloat, 3> rotation)
{
	this->EmitEvent("rotated", nlohmann::json::array({
		nlohmann::json::array({ this->m_rotation.at(0), this->m_rotation.at(1), this->m_rotation.at(2) }),
		nlohmann::json::array({ rotation.at(0), rotation.at(1), rotation.at(2) })
		}));

	this->m_rotation = rotation;
	this->m_rotated = true;
}

void Rotatable::SetRotation(int index, GLfloat value)
{
	std::array<GLfloat, 3> old_rotation = this->m_rotation;

	this->m_rotation.at(index) = value;
	this->m_rotated = true;

	this->EmitEvent("rotated", nlohmann::json::array({
		nlohmann::json::array({ old_rotation.at(0), old_rotation.at(1), old_rotation.at(2) }),
		nlohmann::json::array({ this->m_rotation.at(0), this->m_rotation.at(1), this->m_rotation.at(2) })
		}));
}

void Rotatable::SetRotation(glm::vec3 rotation)
{
	this->EmitEvent("rotated", nlohmann::json::array({
		nlohmann::json::array({ this->m_rotation.at(0), this->m_rotation.at(1), this->m_rotation.at(2) }),
		nlohmann::json::array({ rotation.x, rotation.y, rotation.z })
		}));

	this->m_rotation.at(0) = rotation.x;
	this->m_rotation.at(1) = rotation.y;
	this->m_rotation.at(2) = rotation.z;
}

std::array<GLfloat, 3> Rotatable::GetRotation()
{
	return this->m_rotation;
}

GLfloat Rotatable::GetRotation(int index)
{
	return this->m_rotation.at(index);
}

glm::vec3 Rotatable::GetRotationVec()
{
	return glm::vec3(this->m_rotation.at(0), this->m_rotation.at(1), this->m_rotation.at(2));
}
