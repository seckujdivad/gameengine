#pragma once

#include "../../GLComponents.h"

#include <variant>

enum class GLTextureType;

struct GLTextureFormat_Colour
{
	GLTextureFormat_Colour(int num_channels, GLTextureType type, int bit_depth = 0);

	int num_channels;
	GLTextureType type;
	int bit_depth;

	bool operator==(const GLTextureFormat_Colour& compare) const;
	bool operator!=(const GLTextureFormat_Colour& compare) const;
};

struct GLTextureFormat_Depth
{
	bool operator==(const GLTextureFormat_Depth& compare) const;
	bool operator!=(const GLTextureFormat_Depth& compare) const;
};

using GLTextureFormat = std::variant<GLTextureFormat_Colour, GLTextureFormat_Depth>;

GLint GetTextureFormatEnum(GLTextureFormat format);