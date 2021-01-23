#pragma once

#include <vector>
#include <variant>

#include <glm/glm.hpp>

#include "RenderTextureData.h"
#include "RenderTargetMode.h"

class Camera;

struct RenderTargetConfig
{
	struct Normal_FirstPass
	{

	};

	struct Normal_LastPass
	{
		RenderTextureGroup first_pass;
		RenderTextureGroup pointlight_pass;
		Camera* camera;
		bool draw_reflections = true;
	};

	struct Normal_PointLight
	{
		RenderTextureGroup first_pass;
		bool draw_shadows = true;
	};

	struct Wireframe
	{
		bool draw_back_faces = true;
	};

	struct Shadow
	{

	};

	struct PostProcess
	{
		struct CompositeLayer
		{
			GLuint id = NULL;
			glm::vec4 colour_translate = glm::vec4(0.0f);
			glm::vec4 colour_scale = glm::vec4(1.0f);
		};
		std::vector<CompositeLayer> layers;
	};

	struct Textured
	{

	};

	RenderTargetMode mode = RenderTargetMode::Default;
	std::variant<Normal_FirstPass, Normal_LastPass, Normal_PointLight, Wireframe, Shadow, PostProcess, Textured> mode_data;

	bool clear_fbo = true;

	void SetMode(RenderTargetMode mode);
};

void SetMode(RenderTargetConfig& config, RenderTargetMode mode);