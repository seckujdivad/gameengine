#include <wx/wxprec.h>
#include "Camera.h"

Camera::Camera() : LocallyMovable(), Nameable()
{
	this->m_fov = (GLfloat)45;
}

Camera::~Camera()
{

}

void Camera::SetFOV(GLfloat fov)
{
	this->m_fov = fov;
}

GLfloat Camera::GetFOV()
{
	return this->m_fov;
}

void Camera::SetNearClip(GLfloat nearclip)
{
	this->m_clip_near = nearclip;
}

GLfloat Camera::GetNearClip()
{
	return this->m_clip_near;
}

void Camera::SetFarClip(GLfloat farclip)
{
	this->m_clip_far = farclip;
}

GLfloat Camera::GetFarClip()
{
	return this->m_clip_far;
}

void Camera::GenViewMat()
{
	this->view_translate_vector = glm::vec4(0 - this->GetPosition(0), 0 - this->GetPosition(1), 0 - this->GetPosition(2), 0.0f);

	this->view_rotate_matrix = glm::mat4(1.0f);
	this->view_rotate_matrix = glm::rotate(this->view_rotate_matrix, glm::radians(0 - this->GetRotation(0)), glm::vec3(1.0f, 0.0f, 0.0f));
	this->view_rotate_matrix = glm::rotate(this->view_rotate_matrix, glm::radians(0 - this->GetRotation(1)), glm::vec3(0.0f, 1.0f, 0.0f));
	this->view_rotate_matrix = glm::rotate(this->view_rotate_matrix, glm::radians(0 - this->GetRotation(2)), glm::vec3(0.0f, 0.0f, 1.0f));
}

void Camera::GenPerspMat(float window_width, float window_height)
{
	this->perspective_matrix = glm::perspective(glm::radians(this->m_fov), window_width / window_height, this->m_clip_near, this->m_clip_far);
}

void Camera::RegisterUniforms(ShaderProgram* shader_program)
{
	shader_program->RegisterUniform("cam_translate");
	shader_program->RegisterUniform("cam_rotate");
	shader_program->RegisterUniform("cam_persp");
	shader_program->RegisterUniform("cam_clip_near");
	shader_program->RegisterUniform("cam_clip_far");
}

void Camera::SetUniforms(ShaderProgram* shader_program)
{
	glUniform4fv(shader_program->GetUniform("cam_translate"), 1, glm::value_ptr(this->view_translate_vector));
	glUniformMatrix4fv(shader_program->GetUniform("cam_rotate"), 1, GL_FALSE, glm::value_ptr(this->view_rotate_matrix));
	glUniformMatrix4fv(shader_program->GetUniform("cam_persp"), 1, GL_FALSE, glm::value_ptr(this->perspective_matrix));
	glUniform1f(shader_program->GetUniform("cam_clip_near"), this->m_clip_near);
	glUniform1f(shader_program->GetUniform("cam_clip_far"), this->m_clip_far);
}