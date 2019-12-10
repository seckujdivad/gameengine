#pragma once

#include <wx/glcanvas.h>
#include <tuple>

class Camera
{
private:
	std::tuple<GLfloat, GLfloat, GLfloat> m_position;
	std::tuple<GLfloat, GLfloat, GLfloat> m_rotation;
public:
	Camera();
	~Camera();

	std::tuple<GLfloat, GLfloat, GLfloat>* GetPosition();
	GLfloat GetPosition(int index);
	void SetPosition(std::tuple<GLfloat, GLfloat, GLfloat> position);
	void SetPosition(int index, GLfloat value);

	std::tuple<GLfloat, GLfloat, GLfloat>* GetRotation();
	GLfloat GetRotation(int index);
	void SetRotation(std::tuple<GLfloat, GLfloat, GLfloat> rotation);
	void SetRotation(int index, GLfloat value);
};

