#include <wx/wxprec.h>
#include "Camera.h"

Camera::Camera() : Entity()
{
	this->m_fov = (GLfloat)45;

	this->perspective_matrix = glm::mat4(1.0f);
	this->view_matrix = glm::mat4(1.0f);
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
	this->view_matrix = glm::mat4(1.0f);
	this->view_matrix = glm::translate(this->view_matrix, glm::vec3(0 - this->GetPosition(0), 0 - this->GetPosition(1), 0 - this->GetPosition(2)));
	this->view_matrix = glm::rotate(this->view_matrix, glm::radians(0 - this->GetRotation(0)), glm::vec3(1.0f, 0.0f, 0.0f));
	this->view_matrix = glm::rotate(this->view_matrix, glm::radians(0 - this->GetRotation(1)), glm::vec3(0.0f, 1.0f, 0.0f));
	this->view_matrix = glm::rotate(this->view_matrix, glm::radians(0 - this->GetRotation(2)), glm::vec3(0.0f, 0.0f, 1.0f));
}

void Camera::GenPerspMat(float window_width, float window_height)
{
	this->perspective_matrix = glm::perspective(glm::radians(this->m_fov), window_width / window_height, this->m_clip_near, this->m_clip_far);
}

void Camera::RegisterUniforms(ShaderProgram* shader_program)
{
	shader_program->RegisterUniform("view");
	shader_program->RegisterUniform("projection");
}

void Camera::SetUniforms(ShaderProgram* shader_program)
{
	glUniformMatrix4fv(shader_program->GetUniform("view"), 1, GL_FALSE, glm::value_ptr(this->view_matrix));
	glUniformMatrix4fv(shader_program->GetUniform("projection"), 1, GL_FALSE, glm::value_ptr(this->perspective_matrix));
}