#pragma once

#include "../../GLComponents.h"

enum class TextureFormat
{
	Depth,
	R,
	RG,
	RGB,
	RGBA
};

TextureFormat GetTextureFormat(GLint format);
GLint GetTextureFormatEnum(TextureFormat format);