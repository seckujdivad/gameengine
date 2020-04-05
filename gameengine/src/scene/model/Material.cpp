#include <wx/wxprec.h>
#include "Material.h"

Material::Material()
{
	this->m_diffuse = glm::vec3(0.0f);
	this->m_specular = glm::vec3(0.0f);
	this->m_specular_highlight = 2.0f;
}

Material::~Material()
{

}

void Material::SetDiffuse(glm::vec3 diffuse)
{
	this->m_diffuse = diffuse;
}

glm::vec3 Material::GetDiffuse()
{
	return this->m_diffuse;
}

void Material::SetSpecular(glm::vec3 specular)
{
	this->m_specular = specular;
}

glm::vec3 Material::GetSpecular()
{
	return this->m_specular;
}

void Material::SetSpecularHighlight(float intensity)
{
	this->m_specular_highlight = intensity;
}

float Material::GetSpecularHighlight()
{
	return this->m_specular_highlight;
}

void Material::SetReflection(Reflection* reflection, int mode)
{
	this->m_reflection = reflection;
	this->m_reflection_mode = mode;
}

Reflection* Material::GetReflection()
{
	return this->m_reflection;
}

void Material::SetReflectionIntensity(glm::vec3 intensity)
{
	this->m_reflection_intensity = intensity;
}

glm::vec3 Material::GetReflectionIntensity()
{
	return this->m_reflection_intensity;
}

void Material::RegisterUniforms(ShaderProgram* shader_program)
{
	shader_program->RegisterUniform("mat_diffuse");
	shader_program->RegisterUniform("mat_specular");
	shader_program->RegisterUniform("mat_specular_highlight");
	shader_program->RegisterUniform("mat_reflection_intensity");
	shader_program->RegisterUniform("mat_reflection_mode");

	this->m_reflection->RegisterUniforms(shader_program);
}

void Material::SetUniforms(ShaderProgram* shader_program)
{
	glUniform3fv(shader_program->GetUniform("mat_diffuse"), 1, glm::value_ptr(this->m_diffuse));
	glUniform3fv(shader_program->GetUniform("mat_specular"), 1, glm::value_ptr(this->m_specular));
	glUniform1f(shader_program->GetUniform("mat_specular_highlight"), this->m_specular_highlight);
	glUniform3fv(shader_program->GetUniform("mat_reflection_intensity"), 1, glm::value_ptr(this->m_reflection_intensity));
	glUniform1i(shader_program->GetUniform("mat_reflection_mode"), this->m_reflection_mode);

	this->m_reflection->SetUniforms(shader_program);
}