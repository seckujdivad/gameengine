#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <array>

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "../EventEmitter.h"
#include "../EventManager.h"

class Positionable : public virtual EventEmitter
{
private:
	std::array<GLfloat, 3> m_position = { 0.0f, 0.0f, 0.0f };

	bool m_repositioned = true;

public:
	Positionable(EventManager* evtman);
	Positionable(const Positionable& copyfrom);
	Positionable& operator=(Positionable& copyfrom);
	~Positionable();

	void SetPosition(GLfloat x, GLfloat y, GLfloat z);
	void SetPosition(std::array<GLfloat, 3> point);
	void SetPosition(int index, GLfloat value);
	void SetPosition(glm::vec3 position);
	std::array<GLfloat, 3> GetPosition();
	GLfloat GetPosition(int index);
	glm::vec3 GetPositionVec();

	bool CheckIfRepositioned(bool reset = true);
};