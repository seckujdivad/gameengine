#pragma once

#include <wx/glcanvas.h>
#include <array>

class Camera
{
private:
	std::array<GLfloat, 3>* m_position;
	std::array<GLfloat, 3>* m_rotation;
public:
	Camera();
	~Camera();

	std::array<GLfloat, 3> GetPosition();
	GLfloat GetPosition(int index);
	void SetPosition(std::array<GLfloat, 3> position);
	void SetPosition(int index, GLfloat value);

	std::array<GLfloat, 3> GetRotation();
	GLfloat GetRotation(int index);
	void SetRotation(std::array<GLfloat, 3> rotation);
	void SetRotation(int index, GLfloat value);
};

