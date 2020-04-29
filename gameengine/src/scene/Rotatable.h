#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <array>

class Rotatable
{
private:
	std::array<GLfloat, 3> m_rotation = { 0.0f, 0.0f, 0.0f };

	bool m_rotated = true;

public:
	Rotatable();
	Rotatable(Rotatable& copyfrom);
	Rotatable& operator=(Rotatable& copyfrom);
	~Rotatable();

	void SetRotation(GLfloat x, GLfloat y, GLfloat z);
	void SetRotation(std::array<GLfloat, 3> rotation);
	void SetRotation(int index, GLfloat value);
	std::array<GLfloat, 3> GetRotation();
	GLfloat GetRotation(int index);

	bool CheckIfRotated(bool reset = true);
};