#include "GLTextureDataPreset.h"

#include <stdexcept>

std::string GLTextureDataPresetToString(GLTextureDataPreset preset)
{
	switch (preset)
	{
	case GLTextureDataPreset::Black: return "black";
	case GLTextureDataPreset::ZeroDepth: return "zero depth";
	case GLTextureDataPreset::ZeroShadow: return "zero shadow";
	default: throw std::invalid_argument("Unknown GLTextureDataPreset: " + std::to_string(static_cast<int>(preset)));
	}
}
