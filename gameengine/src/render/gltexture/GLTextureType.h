#pragma once

#include "../../GLComponents.h"

enum class GLTextureType
{
	Float,
	HalfFloat,
	UnsignedByte,
	Integer,
};

GLTextureType GetTextureType(GLenum type);
GLenum GetTextureTypeEnum(GLTextureType type);