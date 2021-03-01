#pragma once

#include "../../GLComponents.h"

enum class GLTextureFormat
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

GLTextureFormat GetTextureFormat(GLint format);
GLint GetTextureFormatEnum(GLTextureFormat format);