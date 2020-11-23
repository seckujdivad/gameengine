#pragma once

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
	RenderTextureGroup texture;
};

struct TexturedRenderModeData
{
	glm::vec3 sun_angle = glm::vec3(1.0f, 0.0f, 1.0f);
};