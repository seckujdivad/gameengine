#pragma once

#include <wx/wxprec.h>
#include "../../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "../Positionable.h"
#include "../Nameable.h"
#include "../../render/ShaderProgram.h"

class PointLight : public Positionable, public Nameable
{
private:
	glm::vec3 m_intensity;

	int m_light_index;

	bool m_shadows_enabled = false;
	GLuint m_depth_cubemap = NULL;
	GLuint m_depth_cubemap_static = NULL;
	GLuint m_depth_fbo = NULL;
	unsigned int m_shadowtex_width = 1024;
	unsigned int m_shadowtex_height = 1024;
	std::vector<glm::mat4> m_transforms;

	unsigned int m_refresh_frames = 0;
	unsigned int m_frames_since_last_refresh = 0;

	float m_shadow_clip_near = 0.1f;
	float m_shadow_clip_far = 100.0f;
	float m_shadow_bias = -0.05f;
	bool m_clips_changed = true;

	std::vector<std::string> m_shadow_static_models;
	std::vector<std::string> m_shadow_dynamic_models;

	void UpdateTransforms();

public:
	PointLight(int light_index, int refresh_frames);
	~PointLight();

	void SetIntensity(glm::vec3 intensity);
	void EnableShadows(unsigned int shadow_texture_width = 1024, unsigned int shadow_texture_height = 1024, float near_plane = 0.1f, float far_plane = 50.0f);
	bool ShadowsEnabled();
	void CreateShadowTextures(unsigned int shadow_texture_width = 1024, unsigned int shadow_texture_height = 1024);

	void InitialiseViewport();
	void SelectFBO();
	void CopyStaticToDynamic();
	void CopyDynamicToStatic();

	void SetNearClip(float near_clip);
	float GetNearClip();

	void SetFarClip(float far_clip);
	float GetFarClip();

	void RegisterShadowUniforms(ShaderProgram* shader_program);
	void SetShadowUniforms(ShaderProgram* shader_program);

	void RegisterUniforms(ShaderProgram* shader_program);
	void SetUniforms(ShaderProgram* shader_program, int mode);

	void AddStaticModel(std::string identifier);
	void RemoveStaticModel(std::string identifier);
	bool ModelIsStatic(std::string identifier);

	void AddDynamicModel(std::string identifier);
	void RemoveDynamicModel(std::string identifier);
	bool ModelIsDynamic(std::string identifier);

	void IncrementFrameCounter(int increment = 1);
	bool DynamicNeedsRedrawing(bool reset_if_redraw = true);
};