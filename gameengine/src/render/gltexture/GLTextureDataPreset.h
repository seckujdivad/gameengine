#pragma once

#include <string>

enum class GLTextureDataPreset
{
	Black,
	ZeroDepth,
	ZeroShadow
};

std::string GLTextureDataPresetToString(GLTextureDataPreset preset);