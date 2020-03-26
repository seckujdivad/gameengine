#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <array>
#include <string>

#include "Positionable.h"
#include "Rotatable.h"
#include "Nameable.h"
#include "../render/ShaderProgram.h"

class Camera : public Positionable, public Rotatable, public Nameable
{
private:
	std::string m_identifier;
	GLfloat m_fov = 45.0f;

	GLfloat m_clip_near = 0.1f;
	GLfloat m_clip_far = 100.0f;

public:
	glm::mat4 view_rotate_matrix;
	glm::vec4 view_translate_vector;
	glm::mat4 perspective_matrix;

	Camera();
	~Camera();

	void SetFOV(GLfloat fov);
	GLfloat GetFOV();

	void SetNearClip(GLfloat nearclip);
	GLfloat GetNearClip();

	void SetFarClip(GLfloat farclip);
	GLfloat GetFarClip();

	void GenViewMat();
	void GenPerspMat(float window_width, float window_height);

	void RegisterUniforms(ShaderProgram* shader_program);
	void SetUniforms(ShaderProgram* shader_program);
};