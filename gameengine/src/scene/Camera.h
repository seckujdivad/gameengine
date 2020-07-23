#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <tuple>

#include "LocallyMovable.h"
#include "Nameable.h"
#include "../EventManager.h"
#include "../EventEmitter.h"

class Camera : public LocallyMovable, public Nameable, public virtual EventEmitter
{
private:
	double m_fov = 45.0;
	std::tuple<double, double> m_clips = { 0.1, 100.0 };
	std::tuple<int, int> m_viewport_dimensions = { 1, 1 };

public:
	Camera(EventManager* evtman);

	void SetFOV(double fov);
	double GetFOV();

	void SetClips(std::tuple<double, double> clips);
	std::tuple<double, double> GetClips();

	void SetViewportDimensions(std::tuple<int, int> dimensions);
	std::tuple<int, int> GetViewportDimensions();

#pragma warning(disable: 4250)
	using Nameable::GetIdentifier;
};
#pragma warning(default: 4250)