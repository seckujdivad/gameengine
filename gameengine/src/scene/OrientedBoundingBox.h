#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "Positionable.h"
#include "Rotatable.h"
#include "Scalable.h"
#include "Nameable.h"
#include "../EventEmitter.h"
#include "../EventManager.h"

class OrientedBoundingBox : public Nameable, public Positionable, public Rotatable, public Scalable, public virtual EventEmitter
{
private:
	glm::mat3 m_rotation_matrix = glm::mat3(1.0f);
	glm::mat3 m_rotation_inverse_matrix = glm::mat3(1.0f);

public:
	OrientedBoundingBox(EventManager* evtman);

	bool PointInBounds(glm::vec3 point);
	bool PointInBounds(float x, float y, float z);

	glm::vec3 GetDimensionsVec();
	glm::mat3 GetRotationMatrix();
	glm::mat3 GetInverseRotationMatrix();

#pragma warning(disable: 4250)
	using Nameable::GetIdentifier;
};
#pragma warning(default: 4250)