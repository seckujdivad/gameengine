#include <wx/wxprec.h>
#include "Camera.h"

Camera::Camera()
{
	this->m_position = new std::array<GLfloat, 3>;
	this->m_rotation = new std::array<GLfloat, 3>;

	this->m_identifier = "";
	this->m_fov = (GLfloat)45;
}

Camera::~Camera()
{
	delete this->m_position;
	delete this->m_rotation;
}

void Camera::SetIdentifier(std::string identifier)
{
	this->m_identifier = identifier;
}

std::string Camera::GetIdentifier()
{
	return this->m_identifier;
}

void Camera::SetFOV(GLfloat fov)
{
	this->m_fov = fov;
}

GLfloat Camera::GetFOV()
{
	return this->m_fov;
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

void Camera::SetPosition(GLfloat x, GLfloat y, GLfloat z)
{
	this->m_position->at(0) = x;
	this->m_position->at(1) = y;
	this->m_position->at(2) = z;
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

void Camera::SetRotation(GLfloat x_rot, GLfloat y_rot, GLfloat z_rot)
{
	this->m_rotation->at(0) = x_rot;
	this->m_rotation->at(1) = y_rot;
	this->m_rotation->at(2) = z_rot;
}
