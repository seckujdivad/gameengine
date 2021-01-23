#include "RenderTargetConfig.h"

#include <stdexcept>
#include <string>

void RenderTargetConfig::SetMode(RenderTargetMode mode)
{
	::SetMode(*this, mode);
}

void SetMode(RenderTargetConfig& config, RenderTargetMode mode)
{
	switch (mode)
	{
	case RenderTargetMode::Default: throw std::invalid_argument("Can't set mode to Default");
	case RenderTargetMode::Normal: config.mode_data = RenderTargetConfig::Normal(); break;
	case RenderTargetMode::Wireframe: config.mode_data = RenderTargetConfig::Wireframe(); break;
	case RenderTargetMode::Shadow: config.mode_data = RenderTargetConfig::Shadow(); break;
	case RenderTargetMode::Postprocess: config.mode_data = RenderTargetConfig::PostProcess(); break;
	case RenderTargetMode::Textured: config.mode_data = RenderTargetConfig::Textured(); break;
	default: throw std::invalid_argument("Unrecognised mode " + std::to_string(static_cast<int>(mode)));
	}

	config.mode = mode;
}
