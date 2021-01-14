#include "RenderJobType.h"

#include <stdexcept>
#include <string>

RenderJobType GetRenderJobType(RenderMode mode)
{
	switch (mode)
	{
	case RenderMode::Normal: return RenderJobType::Normal;
	case RenderMode::Wireframe: return RenderJobType::Wrapper;
	case RenderMode::Shadow: return RenderJobType::Wrapper;
	case RenderMode::Textured: return RenderJobType::Wrapper;
	default: throw std::invalid_argument("Unrecognised RenderMode " + std::to_string(static_cast<int>(mode)));
	}
}
