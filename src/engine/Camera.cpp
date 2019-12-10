#include "Camera.h"

Camera::Camera()
{
	this->m_position = new std::array<GLfloat, 3>;
	this->m_rotation = new std::array<GLfloat, 3>;
}

Camera::~Camera()
{
	delete this->m_position;
	delete this->m_rotation;
}

std::array<GLfloat, 3> Camera::GetPosition()
{
	return *this->m_position;
}

GLfloat Camera::GetPosition(int index)
{
	return this->m_position->at(index);
}

void Camera::SetPosition(std::array<GLfloat, 3> position)
{
	for (int i = 0; i < 3; i++)
	{
		this->m_position->at(i) = position.at(i);
	}
}

void Camera::SetPosition(int index, GLfloat value)
{
	this->m_position->at(index) = value;
}

std::array<GLfloat, 3> Camera::GetRotation()
{
	return *this->m_rotation;
}

GLfloat Camera::GetRotation(int index)
{
	return this->m_rotation->at(index);
}

void Camera::SetRotation(std::array<GLfloat, 3> rotation)
{
	for (int i = 0; i < 3; i++)
	{
		this->m_rotation->at(i) = rotation.at(i);
	}
}

void Camera::SetRotation(int index, GLfloat value)
{
	this->m_rotation->at(index) = value;
}
