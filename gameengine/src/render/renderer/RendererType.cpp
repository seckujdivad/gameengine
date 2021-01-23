#include "RendererType.h"

#include <stdexcept>
#include <string>

RendererType GetRendererType(RenderMode mode)
{
	switch (mode)
	{
	case RenderMode::Normal: return RendererType::Normal;
	case RenderMode::Wireframe: return RendererType::Wrapper;
	case RenderMode::Shadow: return RendererType::Wrapper;
	case RenderMode::Textured: return RendererType::Wrapper;
	default: throw std::invalid_argument("Unrecognised RenderMode " + std::to_string(static_cast<int>(mode)));
	}
}
