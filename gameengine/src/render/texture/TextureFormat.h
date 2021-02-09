#pragma once

#include "../../GLComponents.h"

enum class TextureFormat
{
	Depth,
	R,
	RG,
	RGB,
	RGBA,
	RGB8
};

TextureFormat GetTextureFormat(GLint format);
GLint GetTextureFormatEnum(TextureFormat format);