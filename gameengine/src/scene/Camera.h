#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <array>
#include <string>

#include "LocallyMovable.h"
#include "Nameable.h"
#include "../render/ShaderProgram.h"

class Camera : public LocallyMovable, public Nameable
{
private:
	std::string m_identifier;
	GLfloat m_fov = 45.0f;

	GLfloat m_clip_near = 0.1f;
	GLfloat m_clip_far = 100.0f;

	bool m_persp_transforms_need_update = true;

	int m_window_dimensions[2] = { 1, 1 };

public:
	glm::mat4 view_rotate_matrix = glm::mat4(1.0f);
	glm::vec4 view_translate_vector = glm::vec4(0.0f);
	glm::mat4 perspective_matrix = glm::mat4(1.0f);
	glm::mat4 transform_matrix = glm::mat4(1.0f);
	glm::mat4 transform_inverse_matrix = glm::mat4(1.0f);

	Camera();
	~Camera();

	void SetFOV(GLfloat fov);
	GLfloat GetFOV();

	void SetNearClip(GLfloat nearclip);
	GLfloat GetNearClip();

	void SetFarClip(GLfloat farclip);
	GLfloat GetFarClip();

	void SetViewportDimensions(int width, int height);

	void RegisterUniforms(ShaderProgram* shader_program);
	void SetUniforms(ShaderProgram* shader_program);
};