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
	case RenderTargetMode::Normal_SSRQuality:
	case RenderTargetMode::Normal_PostProcess:
	case RenderTargetMode::PostProcess:
		return RenderTargetModeType::FSQuad;
	default:
		throw std::invalid_argument("Unknown render target mode");
	}
}

std::string GetRenderTargetModeName(RenderTargetMode mode)
{
	switch (mode)
	{
	case RenderTargetMode::Default: return "Default";
	case RenderTargetMode::Normal_DepthOnly: return "Normal_DepthOnly";
	case RenderTargetMode::Normal_Draw: return "Normal_Draw";
	case RenderTargetMode::Normal_PostProcess: return "Normal_PostProcess";
	case RenderTargetMode::Normal_SSRQuality: return "Normal_SSRQuality";
	case RenderTargetMode::PostProcess: return "PostProcess";
	case RenderTargetMode::Shadow: return "Shadow";
	case RenderTargetMode::Textured: return "Textured";
	case RenderTargetMode::Wireframe: return "Wireframe";
	default: throw std::invalid_argument("Unknown render target mode " + std::to_string(static_cast<int>(mode)));
	}
}
