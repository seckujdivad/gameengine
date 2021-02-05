#pragma once

#include "../../GLComponents.h"

enum class TextureType
{
	Float,
	HalfFloat,
	UnsignedByte
};

TextureType GetTextureType(GLenum type);
GLenum GetTextureTypeEnum(TextureType type);