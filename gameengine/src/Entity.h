#pragma once

#include <vector>
#include <string>

#include "GLComponents.h"

class Entity
{
private:
	std::vector<GLfloat> m_position;
	std::vector<GLfloat> m_rotation;
	std::vector<GLfloat> m_scale;

	std::string m_identifier;

public:
	Entity();
	Entity(Entity& copyfrom);
	~Entity();

	void SetIdentifier(std::string identifier);
	std::string GetIdentifier();

	void SetPosition(GLfloat x, GLfloat y, GLfloat z);
	void SetPosition(std::vector<GLfloat> point);
	void SetPosition(int index, GLfloat value);
	std::vector<GLfloat> GetPosition();
	GLfloat GetPosition(int index);

	void SetRotation(GLfloat x, GLfloat y, GLfloat z);
	void SetRotation(std::vector<GLfloat> rotation);
	void SetRotation(int index, GLfloat value);
	std::vector<GLfloat> GetRotation();
	GLfloat GetRotation(int index);

	void SetScale(GLfloat x, GLfloat y, GLfloat z);
	void SetScale(std::vector<GLfloat> scale);
	void SetScale(int index, GLfloat value);
	std::vector<GLfloat> GetScale();
	GLfloat GetScale(int index);
};