#pragma once

#include <glm/glm.hpp>
#include <glm/ext.hpp>

class Positionable
{
private:
	glm::dvec3 m_position = glm::dvec3(0.0);

	bool m_repositioned = true;

public:
	Positionable();

	void SetPosition(glm::dvec3 position);
	glm::dvec3 GetPosition() const;

	glm::dmat4 GetTranslationMatrix() const;
	glm::dmat4 GetTranslationMatrixInverse() const;

	bool CheckIfRepositioned(bool reset = true);
};

glm::dmat4 GetTranslationMatrix(glm::dvec3 translation);