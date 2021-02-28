#pragma once

#include "../../GLComponents.h"

enum class TextureFormat
{
	Depth,
	R,
	RG,
	RGB,
	RGBA,
	RGB8,
	RGBA16F,
	RGBA32F
};

TextureFormat GetTextureFormat(GLint format);
GLint GetTextureFormatEnum(TextureFormat format);