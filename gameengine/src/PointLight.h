#pragma once

#include <wx/wxprec.h>
#include "GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "Entity.h"
#include "render/ShaderProgram.h"

class PointLight : public Entity
{
private:
	glm::vec3 m_intensity;

	int m_light_index;

	bool m_shadows_enabled = false;
	GLuint m_depth_cubemap = NULL;
	GLuint m_depth_fbo = NULL;
	unsigned int m_shadowtex_width = 1024;
	unsigned int m_shadowtex_height = 1024;
	std::vector<glm::mat4> m_transforms;

	float m_shadow_clip_near = 0.1f;
	float m_shadow_clip_far = 25.0f;
	float m_shadow_bias = 0.1f;

public:
	PointLight(int light_index);
	~PointLight();

	void SetIntensity(glm::vec3 intensity);
	void EnableShadows(unsigned int shadow_texture_width = 1024, unsigned int shadow_texture_height = 1024);
	bool ShadowsEnabled();

	void InitialiseViewport();
	void SelectFBO();
	void RegisterShadowUniforms(ShaderProgram* shader_program);
	void SetShadowUniforms(ShaderProgram* shader_program);

	void RegisterUniforms(ShaderProgram* shader_program);
	void SetUniforms(ShaderProgram* shader_program);
};