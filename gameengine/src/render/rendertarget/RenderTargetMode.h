#pragma once

enum class RenderTargetMode
{
	Default,
	Normal_FirstPass,
	Normal_LastPass,
	Normal_PointLight,
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