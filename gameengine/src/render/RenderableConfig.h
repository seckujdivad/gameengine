#pragma once

#include <vector>
#include <variant>

#include <glm/glm.hpp>

#include "RenderTextureData.h"
#include "RenderMode.h"

struct RenderableConfig
{
	struct Normal
	{
		RenderTextureGroup previous_frame;
		bool draw_shadows = true;
	};

	struct Wireframe
	{

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

	RenderMode mode = RenderMode::Default;
	std::variant<Normal, Wireframe, Shadow, PostProcess, Textured> mode_data;

	bool clear_fbo = true;
};