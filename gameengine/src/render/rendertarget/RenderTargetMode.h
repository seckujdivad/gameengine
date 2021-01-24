#pragma once

enum class RenderTargetMode
{
	Default,
	Normal_DepthOnly,
	Normal_Draw,
	Wireframe,
	Shadow,
	Postprocess,
	Textured
};

enum class RenderTargetModeType
{
	Model,
	FSQuad //full screen quad
};

RenderTargetModeType GetRenderTargetModeType(RenderTargetMode mode);