#include <wx/wxprec.h>
#include "Camera.h"

Camera::Camera(EventManager* evtman) : LocallyMovable(evtman), Nameable(evtman), EventEmitter(evtman)
{
	this->m_fov = (GLfloat)45;
}

Camera::~Camera()
{

}

void Camera::SetFOV(GLfloat fov)
{
	this->m_fov = fov;

	this->m_persp_transforms_need_update = true;
}

GLfloat Camera::GetFOV()
{
	return this->m_fov;
}

void Camera::SetNearClip(GLfloat nearclip)
{
	this->m_clip_near = nearclip;

	this->m_persp_transforms_need_update = true;
}

GLfloat Camera::GetNearClip()
{
	return this->m_clip_near;
}

void Camera::SetFarClip(GLfloat farclip)
{
	this->m_clip_far = farclip;

	this->m_persp_transforms_need_update = true;
}

GLfloat Camera::GetFarClip()
{
	return this->m_clip_far;
}

void Camera::SetViewportDimensions(int width, int height)
{
	this->m_window_dimensions[0] = width;
	this->m_window_dimensions[1] = height;

	this->m_persp_transforms_need_update = true;
}

void Camera::RegisterUniforms(ShaderProgram* shader_program)
{
	shader_program->RegisterUniform("cam_translate");
	shader_program->RegisterUniform("cam_rotate");
	shader_program->RegisterUniform("cam_persp");
	shader_program->RegisterUniform("cam_clip_near");
	shader_program->RegisterUniform("cam_clip_far");
	shader_program->RegisterUniform("cam_transform");
	shader_program->RegisterUniform("cam_transform_inverse");
}

void Camera::SetUniforms(ShaderProgram* shader_program)
{
	//generate matrices and vectors if required
	bool remake_combined = false;
	
	if (this->CheckIfRepositioned(true))
	{
		this->m_view_translate_vector = glm::vec4(0 - this->GetPosition(0), 0 - this->GetPosition(1), 0 - this->GetPosition(2), 0.0f);

		remake_combined = true;
	}

	if (this->CheckIfRotated(true))
	{
		this->m_view_rotate_matrix = glm::mat4(1.0f);
		this->m_view_rotate_matrix = glm::rotate(this->m_view_rotate_matrix, glm::radians(0 - this->GetRotation(0)), glm::vec3(1.0f, 0.0f, 0.0f));
		this->m_view_rotate_matrix = glm::rotate(this->m_view_rotate_matrix, glm::radians(0 - this->GetRotation(1)), glm::vec3(0.0f, 1.0f, 0.0f));
		this->m_view_rotate_matrix = glm::rotate(this->m_view_rotate_matrix, glm::radians(0 - this->GetRotation(2)), glm::vec3(0.0f, 0.0f, 1.0f));

		remake_combined = true;
	}

	if (this->m_persp_transforms_need_update)
	{
		this->m_perspective_matrix = glm::perspective(glm::radians(this->m_fov), (float)this->m_window_dimensions[0] / (float)this->m_window_dimensions[1], this->m_clip_near, this->m_clip_far);

		remake_combined = true;
	}

	if (remake_combined)
	{
		this->m_transform_matrix = glm::mat4(1.0f);
		this->m_transform_matrix = glm::translate(this->m_transform_matrix, glm::vec3(this->m_view_translate_vector));
		this->m_transform_matrix = this->m_perspective_matrix * this->m_view_rotate_matrix * this->m_transform_matrix;

		this->m_transform_inverse_matrix = glm::inverse(this->m_transform_matrix);
	}

	this->m_persp_transforms_need_update = false;

	//send matrices and vectors to gpu
	glUniform4fv(shader_program->GetUniform("cam_translate"), 1, glm::value_ptr(this->m_view_translate_vector));
	glUniformMatrix4fv(shader_program->GetUniform("cam_rotate"), 1, GL_FALSE, glm::value_ptr(this->m_view_rotate_matrix));
	glUniformMatrix4fv(shader_program->GetUniform("cam_persp"), 1, GL_FALSE, glm::value_ptr(this->m_perspective_matrix));
	glUniform1f(shader_program->GetUniform("cam_clip_near"), this->m_clip_near);
	glUniform1f(shader_program->GetUniform("cam_clip_far"), this->m_clip_far);
	glUniformMatrix4fv(shader_program->GetUniform("cam_transform"), 1, GL_FALSE, glm::value_ptr(this->m_transform_matrix));
	glUniformMatrix4fv(shader_program->GetUniform("cam_transform_inverse"), 1, GL_FALSE, glm::value_ptr(this->m_transform_inverse_matrix));
}