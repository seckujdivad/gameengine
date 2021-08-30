#pragma once

#include <glm/glm.hpp>
#include <glm/ext.hpp>

class Scalable
{
private:
	glm::dvec3 m_scale = glm::dvec3(1.0);

	bool m_rescaled = true;
	
public:
	Scalable();

	void SetScale(glm::dvec3 scale);
	glm::dvec3 GetScale() const;

	glm::dmat4 GetScaleMatrix() const;
	glm::dmat4 GetScaleMatrixInverse() const;

	bool CheckIfRescaled(bool reset = true);
};

glm::dmat4 GetScaleMatrix(glm::dvec3 scale);