#include <wx/wxprec.h>
#include "PointLight.h"

PointLight::PointLight(int light_index)
{
	this->m_diffuse = glm::vec3(0.0f);
	this->m_specular = glm::vec3(0.0f);

	this->m_light_index = light_index;
}

PointLight::~PointLight()
{

}

void PointLight::SetDiffuse(glm::vec3 intensity)
{
	this->m_diffuse = intensity;
}

void PointLight::SetSpecular(glm::vec3 intensity)
{
	this->m_specular = intensity;
}

void PointLight::RegisterUniforms(ShaderProgram* shader_program)
{
	std::string prefix = "light_points[" + std::to_string(this->m_light_index) + "].";
	shader_program->RegisterUniform(prefix + "position");
	shader_program->RegisterUniform(prefix + "diffuse");
	shader_program->RegisterUniform(prefix + "specular");
}

void PointLight::SetUniforms(ShaderProgram* shader_program)
{
	std::string prefix = "light_points[" + std::to_string(this->m_light_index) + "].";

	glm::vec3 position = glm::vec3(this->GetPosition(0), this->GetPosition(1), this->GetPosition(2));
	glUniform3fv(shader_program->GetUniform(prefix + "position"), 1, glm::value_ptr(position));

	glUniform3fv(shader_program->GetUniform(prefix + "diffuse"), 1, glm::value_ptr(this->m_diffuse));
	glUniform3fv(shader_program->GetUniform(prefix + "specular"), 1, glm::value_ptr(this->m_specular));
}