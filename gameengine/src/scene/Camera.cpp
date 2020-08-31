#include "Camera.h"

Camera::Camera() : LocallyMovable(), Nameable()
{
	this->SetFOV(45.0);
}

void Camera::SetFOV(double fov)
{
	this->m_fov = fov;
}

double Camera::GetFOV()
{
	return this->m_fov;
}

void Camera::SetClips(std::tuple<double, double> clips)
{
	this->m_clips = clips;
}

std::tuple<double, double> Camera::GetClips()
{
	return this->m_clips;
}

void Camera::SetViewportDimensions(std::tuple<int, int> dimensions)
{
	this->m_viewport_dimensions = dimensions;
}

std::tuple<int, int> Camera::GetViewportDimensions()
{
	return this->m_viewport_dimensions;
}

glm::dmat4 Camera::GetPerspectiveMatrix()
{
	return glm::perspective(glm::radians(this->m_fov), (double)std::get<0>(this->m_viewport_dimensions) / (double)std::get<1>(this->m_viewport_dimensions), std::get<0>(this->m_clips), std::get<1>(this->m_clips));
}

glm::dmat4 Camera::GetCombinedMatrix()
{
	return this->GetPerspectiveMatrix() * this->GetRotationMatrixInverse() * this->GetTranslationMatrixInverse();
}
