#include <wx/wxprec.h>
#include "Camera.h"

Camera::Camera(EventManager* evtman) : LocallyMovable(evtman), Nameable(evtman), EventEmitter(evtman)
{
	this->m_fov = (GLfloat)45;
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
