#include <wx/wxprec.h>
#include "Material.h"

Material::Material()
{

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

void Material::RegisterUniforms(ShaderProgram* shader_program)
{
	shader_program->RegisterUniform("mat_diffuse");
	shader_program->RegisterUniform("mat_specular");
	shader_program->RegisterUniform("mat_specular_highlight");
	shader_program->RegisterUniform("mat_reflection_mode");

	shader_program->RegisterUniform("mat_ssr_enabled");
	shader_program->RegisterUniform("mat_ssr_samples");
	shader_program->RegisterUniform("mat_ssr_max_distance");
	shader_program->RegisterUniform("mat_ssr_max_cast_distance");
	shader_program->RegisterUniform("mat_ssr_depth_acceptance");

	this->m_reflection->RegisterUniforms(shader_program);
}

void Material::SetUniforms(ShaderProgram* shader_program)
{
	glUniform3fv(shader_program->GetUniform("mat_diffuse"), 1, glm::value_ptr(this->m_diffuse));
	glUniform3fv(shader_program->GetUniform("mat_specular"), 1, glm::value_ptr(this->m_specular));
	glUniform1f(shader_program->GetUniform("mat_specular_highlight"), this->m_specular_highlight);
	glUniform1i(shader_program->GetUniform("mat_reflection_mode"), this->m_reflection_mode);

	glUniform1i(shader_program->GetUniform("mat_ssr_enabled"), this->m_ssr_enabled);
	glUniform1i(shader_program->GetUniform("mat_ssr_samples"), this->m_ssr_config.samples);
	glUniform1f(shader_program->GetUniform("mat_ssr_max_distance"), this->m_ssr_config.max_cam_distance);
	glUniform1f(shader_program->GetUniform("mat_ssr_max_cast_distance"), this->m_ssr_config.cast_distance_limit);
	glUniform1f(shader_program->GetUniform("mat_ssr_depth_acceptance"), this->m_ssr_config.depth_acceptance);

	this->m_reflection->SetUniforms(shader_program);
}

void Material::EnableSSR(bool enable)
{
	this->m_ssr_enabled = enable;
}

bool Material::SSREnabled()
{
	return this->m_ssr_enabled;
}

void Material::SetSSRConfig(MaterialSSRConfig config)
{
	this->m_ssr_config = config;
}

MaterialSSRConfig Material::GetSSRConfig()
{
	return this->m_ssr_config;
}
