#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <array>

class Scalable
{
private:
	std::array<GLfloat, 3> m_scale = { 0.0f, 0.0f, 0.0f };

	bool m_rescaled = true;
	
public:
	Scalable();
	Scalable(Scalable& copyfrom);
	Scalable& operator=(Scalable& copyfrom);
	~Scalable();

	void SetScale(GLfloat x, GLfloat y, GLfloat z);
	void SetScale(std::array<GLfloat, 3> scale);
	void SetScale(int index, GLfloat value);
	std::array<GLfloat, 3> GetScale();
	GLfloat GetScale(int index);

	bool CheckIfRescaled(bool reset = true);
};