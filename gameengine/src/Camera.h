#pragma once

#include <wx/wxprec.h>
#include "GLComponents.h"

#include <array>
#include <string>

#include "Entity.h"


class Camera : public Entity
{
private:
	std::string m_identifier;
	GLfloat m_fov;

public:
	Camera();
	~Camera();

	void SetFOV(GLfloat fov);
	GLfloat GetFOV();
};