#include <wx/wxprec.h>
#include "Camera.h"

Camera::Camera() : Entity()
{
	this->m_fov = (GLfloat)45;
}

Camera::~Camera()
{

}

void Camera::SetFOV(GLfloat fov)
{
	this->m_fov = fov;
}

GLfloat Camera::GetFOV()
{
	return this->m_fov;
}