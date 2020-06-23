#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <array>

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "../EventEmitter.h"
#include "../EventManager.h"

class Rotatable : public virtual EventEmitter
{
private:
	std::array<GLfloat, 3> m_rotation = { 0.0f, 0.0f, 0.0f };

	bool m_rotated = true;

public:
	Rotatable(EventManager* evtman);
	Rotatable(const Rotatable& copyfrom);
	Rotatable& operator=(Rotatable& copyfrom);
	~Rotatable();

	void SetRotation(GLfloat x, GLfloat y, GLfloat z);
	void SetRotation(std::array<GLfloat, 3> rotation);
	void SetRotation(int index, GLfloat value);
	void SetRotation(glm::vec3 rotation);
	std::array<GLfloat, 3> GetRotation();
	GLfloat GetRotation(int index);
	glm::vec3 GetRotationVec();

	bool CheckIfRotated(bool reset = true);
};