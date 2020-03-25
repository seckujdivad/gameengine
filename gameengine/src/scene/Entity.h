#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include "Positionable.h"
#include "Rotatable.h"
#include "Scalable.h"
#include "Nameable.h"

#include <array>
#include <string>


class Entity : public Nameable
{
private:
	std::array<GLfloat, 3> m_position;
	std::array<GLfloat, 3> m_rotation;
	std::array<GLfloat, 3> m_scale;

public:
	Entity();
	Entity(Entity& copyfrom);
	Entity& operator=(Entity& copyfrom);
	~Entity();

	void SetPosition(GLfloat x, GLfloat y, GLfloat z);
	void SetPosition(std::array<GLfloat, 3> point);
	void SetPosition(int index, GLfloat value);
	std::array<GLfloat, 3> GetPosition();
	GLfloat GetPosition(int index);

	void SetRotation(GLfloat x, GLfloat y, GLfloat z);
	void SetRotation(std::array<GLfloat, 3> rotation);
	void SetRotation(int index, GLfloat value);
	std::array<GLfloat, 3> GetRotation();
	GLfloat GetRotation(int index);

	void SetScale(GLfloat x, GLfloat y, GLfloat z);
	void SetScale(std::array<GLfloat, 3> scale);
	void SetScale(int index, GLfloat value);
	std::array<GLfloat, 3> GetScale();
	GLfloat GetScale(int index);
};