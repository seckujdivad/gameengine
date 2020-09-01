#pragma once

#include "RenderTextureData.h"

struct RenderableConfig
{
	bool clear_fbo = true;
};

struct NormalRenderModeData
{
	RenderTextureGroup previous_frame;
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