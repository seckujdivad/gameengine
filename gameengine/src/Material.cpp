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

void Material::RegisterUniforms(ShaderProgram* shader_program)
{
	shader_program->RegisterUniform("mat_diffuse");
	shader_program->RegisterUniform("mat_specular");
	shader_program->RegisterUniform("mat_specular_highlight");
}

void Material::SetUniforms(ShaderProgram* shader_program)
{
	glUniform3fv(shader_program->GetUniform("mat_diffuse"), 1, glm::value_ptr(this->m_diffuse));
	glUniform3fv(shader_program->GetUniform("mat_specular"), 1, glm::value_ptr(this->m_specular));
	glUniform1f(shader_program->GetUniform("mat_specular_highlight"), this->m_specular_highlight);
}