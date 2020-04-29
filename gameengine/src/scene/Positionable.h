#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <array>

class Positionable
{
private:
	std::array<GLfloat, 3> m_position = { 0.0f, 0.0f, 0.0f };

	bool m_repositioned = true;

public:
	Positionable();
	Positionable(Positionable& copyfrom);
	Positionable& operator=(Positionable& copyfrom);
	~Positionable();

	void SetPosition(GLfloat x, GLfloat y, GLfloat z);
	void SetPosition(std::array<GLfloat, 3> point);
	void SetPosition(int index, GLfloat value);
	std::array<GLfloat, 3> GetPosition();
	GLfloat GetPosition(int index);

	bool CheckIfRepositioned(bool reset = true);
};