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
	std::vector<GLuint> textures;
};

struct TexturedRenderModeData
{
	
};