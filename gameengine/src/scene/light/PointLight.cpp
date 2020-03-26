#include <wx/wxprec.h>
#include "PointLight.h"

PointLight::PointLight(int light_index) : Positionable(), Nameable()
{
	this->m_light_index = light_index;

	this->m_intensity = glm::vec3(0.0f);
}

PointLight::~PointLight()
{

}

void PointLight::SetIntensity(glm::vec3 intensity)
{
	this->m_intensity = intensity;
}

void PointLight::EnableShadows(unsigned int shadow_texture_width, unsigned int shadow_texture_height, float near_plane, float far_plane)
{
	if (this->m_shadows_enabled)
	{
		throw std::runtime_error("Can't reenable shadows, shadows are already enabled for this PointLight");
	}

	this->m_shadowtex_width = shadow_texture_width;
	this->m_shadowtex_height = shadow_texture_height;
	this->m_shadows_enabled = true;
	this->m_shadow_clip_near = near_plane;
	this->m_shadow_clip_far = far_plane;

	//make cubemap
	glGenTextures(1, &this->m_depth_cubemap);
	glBindTexture(GL_TEXTURE_CUBE_MAP, this->m_depth_cubemap);
	for (int i = 0; i < 6; i++)
	{
		glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, GL_DEPTH_COMPONENT, this->m_shadowtex_width, this->m_shadowtex_height, 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
	}
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	glGenFramebuffers(1, &this->m_depth_fbo);
	glBindFramebuffer(GL_FRAMEBUFFER, this->m_depth_fbo);
	glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, this->m_depth_cubemap, 0);
	glDrawBuffer(GL_NONE);
	glReadBuffer(GL_NONE);

	GLenum framebuffer_status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
	if (framebuffer_status != GL_FRAMEBUFFER_COMPLETE)
	{
		throw std::runtime_error("Framebuffer error, status " + std::to_string(framebuffer_status));
	}

	glBindFramebuffer(GL_FRAMEBUFFER, 0);

	glm::vec3 translate = glm::vec3(this->GetPosition(0), this->GetPosition(1), this->GetPosition(2));
	glm::mat4 projection = glm::perspective(glm::radians(90.0f), (float)this->m_shadowtex_width / (float)this->m_shadowtex_height, this->m_shadow_clip_near, this->m_shadow_clip_far);
	
	this->m_transforms.clear();
	this->m_transforms.push_back(projection * glm::lookAt(translate, translate + glm::vec3(1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
	this->m_transforms.push_back(projection * glm::lookAt(translate, translate + glm::vec3(-1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
	this->m_transforms.push_back(projection * glm::lookAt(translate, translate + glm::vec3(0.0f, 1.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f)));
	this->m_transforms.push_back(projection * glm::lookAt(translate, translate + glm::vec3(0.0f, -1.0f, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f)));
	this->m_transforms.push_back(projection * glm::lookAt(translate, translate + glm::vec3(0.0f, 0.0f, 1.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
	this->m_transforms.push_back(projection * glm::lookAt(translate, translate + glm::vec3(0.0f, 0.0f, -1.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
}

bool PointLight::ShadowsEnabled()
{
	return this->m_shadows_enabled;
}

void PointLight::RegisterUniforms(ShaderProgram* shader_program)
{
	std::string prefix = "light_points[" + std::to_string(this->m_light_index) + "].";
	shader_program->RegisterUniform(prefix + "position");
	shader_program->RegisterUniform(prefix + "intensity");
	shader_program->RegisterUniform(prefix + "shadows_enabled");
	shader_program->RegisterUniform(prefix + "shadow_cubemap");

	if (this->m_shadows_enabled)
	{
		shader_program->RegisterTexture(prefix + "shadow_cubemap", this->m_depth_cubemap, GL_TEXTURE_CUBE_MAP);
		shader_program->RegisterUniform(prefix + "shadow_bias");
		shader_program->RegisterUniform(prefix + "shadow_far_plane");
	}
}

void PointLight::SetUniforms(ShaderProgram* shader_program)
{
	std::string prefix = "light_points[" + std::to_string(this->m_light_index) + "].";

	glm::vec3 position = glm::vec3(this->GetPosition(0), this->GetPosition(1), this->GetPosition(2));
	glUniform3fv(shader_program->GetUniform(prefix + "position"), 1, glm::value_ptr(position));
	glUniform3fv(shader_program->GetUniform(prefix + "intensity"), 1, glm::value_ptr(this->m_intensity));
	glUniform1i(shader_program->GetUniform(prefix + "shadows_enabled"), this->m_shadows_enabled);

	if (this->m_shadows_enabled)
	{
		glUniform1f(shader_program->GetUniform(prefix + "shadow_bias"), this->m_shadow_bias);
		glUniform1f(shader_program->GetUniform(prefix + "shadow_far_plane"), this->m_shadow_clip_far);
	}
}

void PointLight::InitialiseViewport()
{
	glViewport(0, 0, this->m_shadowtex_width, this->m_shadowtex_height);
}

void PointLight::SelectFBO()
{
	glBindFramebuffer(GL_FRAMEBUFFER, this->m_depth_fbo);
}

void PointLight::RegisterShadowUniforms(ShaderProgram* shader_program)
{
	
	shader_program->RegisterUniform("light_position");
	shader_program->RegisterUniform("light_far_plane");

	for (int i = 0; i < 6; i++)
	{
		shader_program->RegisterUniform("light_transform[" + std::to_string(i) + "]");
	}
}

void PointLight::SetShadowUniforms(ShaderProgram* shader_program)
{
	for (int i = 0; i < 6; i++)
	{
		glUniformMatrix4fv(shader_program->GetUniform("light_transform[" + std::to_string(i) + "]"), 1, GL_FALSE, glm::value_ptr(this->m_transforms.at(i)));
	}
	
	glm::vec3 light_pos = glm::vec3(this->GetPosition(0), this->GetPosition(1), this->GetPosition(2));
	glUniform3fv(shader_program->GetUniform("light_position"), 1, glm::value_ptr(light_pos));

	glUniform1f(shader_program->GetUniform("light_far_plane"), this->m_shadow_clip_far);
}