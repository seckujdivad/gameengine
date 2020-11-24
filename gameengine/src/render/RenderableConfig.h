#pragma once

#include <vector>

#include "RenderTextureData.h"

struct RenderableConfig
{
	bool clear_fbo = true;
};

struct NormalRenderModeData
{
	RenderTextureGroup previous_frame;
	bool draw_shadows = true;
};

struct WireframeRenderModeData
{

};

struct ShadowRenderModeData
{

};

struct PostProcessRenderModeData
{
	struct CompositeLayer
	{
		GLuint id = NULL;
		glm::vec4 colour_translate = glm::vec4(0.0f);
		glm::vec4 colour_scale = glm::vec4(1.0f);
	};
	std::vector<CompositeLayer> layers;
};

struct TexturedRenderModeData
{
	
};