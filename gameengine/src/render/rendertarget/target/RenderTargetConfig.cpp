#include "RenderTargetConfig.h"

#include "../texture/RenderTextureGroup.h"
#include "RenderTargetMode.h"

#include <stdexcept>
#include <string>

RenderTargetConfig::RenderTargetConfig() : mode(RenderTargetMode::Default)
{
}

RenderTargetConfig::RenderTargetConfig(RenderTargetMode mode, ModeData mode_data) : mode(mode), mode_data(mode_data)
{
}

void RenderTargetConfig::SetMode(RenderTargetMode mode)
{
	::SetMode(*this, mode);
}

void SetMode(RenderTargetConfig& config, RenderTargetMode mode)
{
	switch (mode)
	{
	case RenderTargetMode::Default: throw std::invalid_argument("Can't set mode to Default");
	case RenderTargetMode::Normal_DepthOnly: config.mode_data = RenderTargetConfig::Normal_DepthOnly(); break;
	case RenderTargetMode::Normal_Draw: config.mode_data = RenderTargetConfig::Normal_Draw(); break;
	case RenderTargetMode::Normal_SSRQuality: config.mode_data = RenderTargetConfig::Normal_SSRQuality(); break;
	case RenderTargetMode::Normal_PostProcess: config.mode_data = RenderTargetConfig::Normal_PostProcess(); break;
	case RenderTargetMode::Wireframe: config.mode_data = RenderTargetConfig::Wireframe(); break;
	case RenderTargetMode::Shadow: config.mode_data = RenderTargetConfig::Shadow(); break;
	case RenderTargetMode::PostProcess: config.mode_data = RenderTargetConfig::PostProcess(); break;
	case RenderTargetMode::Textured: config.mode_data = RenderTargetConfig::Textured(); break;
	default: throw std::invalid_argument("Unrecognised mode " + std::to_string(static_cast<int>(mode)));
	}

	config.mode = mode;
}