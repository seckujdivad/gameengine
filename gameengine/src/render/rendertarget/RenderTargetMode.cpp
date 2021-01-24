#include "RenderTargetMode.h"

#include <stdexcept>

RenderTargetModeType GetRenderTargetModeType(RenderTargetMode mode)
{
	switch (mode)
	{
	case RenderTargetMode::Normal_DepthOnly:
	case RenderTargetMode::Normal_Draw:
	case RenderTargetMode::Wireframe:
	case RenderTargetMode::Shadow:
	case RenderTargetMode::Textured:
		return RenderTargetModeType::Model;
	case RenderTargetMode::Postprocess:
		return RenderTargetModeType::FSQuad;
	default:
		throw std::invalid_argument("Unknown render target mode");
	}
}
