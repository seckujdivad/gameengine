#pragma once

enum class RenderTargetMode
{
	Default,
	Normal_DepthOnly,
	Normal_Draw,
	Normal_SSRQuality,
	Normal_PostProcess,
	Wireframe,
	Shadow,
	PostProcess,
	Textured
};

enum class RenderTargetModeType
{
	Model,
	FSQuad //full screen quad
};

RenderTargetModeType GetRenderTargetModeType(RenderTargetMode mode);