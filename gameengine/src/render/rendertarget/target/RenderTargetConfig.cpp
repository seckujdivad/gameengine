#include "RenderTargetConfig.h"

#include "../texture/RenderTextureGroup.h"
#include "RenderTargetMode.h"

#include <stdexcept>
#include <string>

RenderTargetConfig::RenderTargetConfig() : mode_data(RenderTargetConfig::Default())
{
}

RenderTargetConfig::RenderTargetConfig(RenderTargetMode mode)
{
	this->SetMode(mode);
}

RenderTargetConfig::RenderTargetConfig(ModeData mode_data) : mode_data(mode_data)
{
}

void RenderTargetConfig::SetMode(RenderTargetMode mode)
{
	switch (mode)
	{
	case RenderTargetMode::Default: throw std::invalid_argument("Can't set mode to Default");
	case RenderTargetMode::Normal_DepthOnly: this->mode_data = RenderTargetConfig::Normal_DepthOnly(); break;
	case RenderTargetMode::Normal_Draw: this->mode_data = RenderTargetConfig::Normal_Draw(); break;
	case RenderTargetMode::Normal_SSRQuality: this->mode_data = RenderTargetConfig::Normal_SSRQuality(); break;
	case RenderTargetMode::Normal_PostProcess: this->mode_data = RenderTargetConfig::Normal_PostProcess(); break;
	case RenderTargetMode::Wireframe: this->mode_data = RenderTargetConfig::Wireframe(); break;
	case RenderTargetMode::Shadow: this->mode_data = RenderTargetConfig::Shadow(); break;
	case RenderTargetMode::PostProcess: this->mode_data = RenderTargetConfig::PostProcess(); break;
	case RenderTargetMode::Textured: this->mode_data = RenderTargetConfig::Textured(); break;
	default: throw std::invalid_argument("Unrecognised mode " + std::to_string(static_cast<int>(mode)));
	}
}

RenderTargetMode RenderTargetConfig::GetMode() const
{
	return static_cast<RenderTargetMode>(this->mode_data.index());
}

RenderTargetConfig::PostProcess::Mode RenderTargetConfig::PostProcess::GetMode() const
{
	return static_cast<RenderTargetConfig::PostProcess::Mode>(this->data.index() - 1);
}
