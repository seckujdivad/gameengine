#pragma once

#include <wx/wxprec.h>
#include "../../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "../Positionable.h"
#include "../Nameable.h"
#include "../../render/ShaderProgram.h"

class Reflection : public Positionable, public Nameable
{
private:
	GLuint m_fbo = NULL;
	GLuint m_cubemap = NULL;
	GLuint m_cubemap_static = NULL;
	unsigned int m_tex_width = 1024;
	unsigned int m_tex_height = 1024;
	float m_clip_near = 0.1f;
	float m_clip_far = 100.0f;

	unsigned int m_refresh_frames = 0;
	unsigned int m_frames_since_last_refresh = 0;

	unsigned int m_parallax_correction_iterations = 1;

	glm::vec4 m_transform_translate; //cam pos
	glm::mat4 m_transform_perspective; //cam perspective
	std::vector<glm::mat4> m_transform_rotate; //cam rotate

	std::vector<std::string> m_models_static;
	std::vector<std::string> m_models_dynamic;

public:
	Reflection(unsigned int texture_width, unsigned int texture_height, float near_plane, float far_plane, unsigned int refresh_frames, unsigned int parallax_correction_iterations);
	~Reflection();

	void InitialiseViewport();
	void SelectFBO(int face);
	void CopyStaticToDynamic();
	void CopyDynamicToStatic();

	void RegisterUniforms(ShaderProgram* shader_program);
	void SetUniforms(ShaderProgram* shader_program);
	void GenerateCameraData();

	void AddStaticModel(std::string identifier);
	void RemoveStaticModel(std::string identifier);
	bool ModelIsStatic(std::string identifier);

	void AddDynamicModel(std::string identifier);
	void RemoveDynamicModel(std::string identifier);
	bool ModelIsDynamic(std::string identifier);

	void RegisterGenerateUniforms(ShaderProgram* shader_program);
	void SetGenerateUniforms(ShaderProgram* shader_program, int face);

	void IncrementFrameCounter(int increment = 1);
	bool DynamicNeedsRedrawing(bool reset_if_redraw = true);
};