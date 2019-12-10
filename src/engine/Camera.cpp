#include "Camera.h"

Camera::Camera()
{

}

Camera::~Camera()
{

}

std::tuple<GLfloat, GLfloat, GLfloat>* Camera::GetPosition()
{
	return nullptr;
}

GLfloat Camera::GetPosition(int index)
{
	return GLfloat();
}

void Camera::SetPosition(std::tuple<GLfloat, GLfloat, GLfloat> position)
{
}

void Camera::SetPosition(int index, GLfloat value)
{
}

std::tuple<GLfloat, GLfloat, GLfloat>* Camera::GetRotation()
{
	return nullptr;
}

GLfloat Camera::GetRotation(int index)
{
	return GLfloat();
}

void Camera::SetRotation(std::tuple<GLfloat, GLfloat, GLfloat> rotation)
{
}

void Camera::SetRotation(int index, GLfloat value)
{
}
