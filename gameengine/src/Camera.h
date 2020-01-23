#pragma once

#include <wx/wxprec.h>
#include "GLComponents.h"

#include <array>
#include <string>


class Camera
{
private:
	std::array<GLfloat, 3>* m_position;
	std::array<GLfloat, 3>* m_rotation;

	std::string m_identifier;
	GLfloat m_fov;

public:
	Camera();
	~Camera();

	void SetIdentifier(std::string identifier);
	std::string GetIdentifier();

	void SetFOV(GLfloat fov);
	GLfloat GetFOV();

	std::array<GLfloat, 3> GetPosition();
	GLfloat GetPosition(int index);
	void SetPosition(std::array<GLfloat, 3> position);
	void SetPosition(int index, GLfloat value);
	void SetPosition(GLfloat x, GLfloat y, GLfloat z);

	std::array<GLfloat, 3> GetRotation();
	GLfloat GetRotation(int index);
	void SetRotation(std::array<GLfloat, 3> rotation);
	void SetRotation(int index, GLfloat value);
	void SetRotation(GLfloat x_rot, GLfloat y_rot, GLfloat z_rot);
};