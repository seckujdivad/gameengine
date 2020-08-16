#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

class Positionable
{
private:
	glm::dvec3 m_position = glm::dvec3(0.0);

	bool m_repositioned = true;

public:
	Positionable();

	void SetPosition(double x, double y, double z);
	void SetPosition(int index, double value);
	void SetPosition(glm::dvec3 position);

	glm::dvec3 GetPosition() const;
	double GetPosition(int index) const;

	bool CheckIfRepositioned(bool reset = true);
};