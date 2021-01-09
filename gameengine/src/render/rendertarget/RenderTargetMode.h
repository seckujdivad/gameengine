#pragma once

enum class RenderTargetMode
{
	Default,
	Normal,
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