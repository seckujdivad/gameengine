#include "TextureType.h"

#include <stdexcept>
#include <string>

TextureType GetTextureType(GLenum type)
{
	switch (type)
	{
	case GL_FLOAT: return TextureType::Float;
	case GL_HALF_FLOAT: return TextureType::HalfFloat;
	case GL_UNSIGNED_BYTE: return TextureType::UnsignedByte;
	case GL_INT: return TextureType::Integer;
	default: throw std::invalid_argument("Unknown texture type " + std::to_string(type));
	}
}

GLenum GetTextureTypeEnum(TextureType type)
{
	switch (type)
	{
	case TextureType::Float: return GL_FLOAT;
	case TextureType::HalfFloat: return GL_HALF_FLOAT;
	case TextureType::UnsignedByte: return GL_UNSIGNED_BYTE;
	case TextureType::Integer: return GL_INT;
	default: throw std::invalid_argument("Unknown texture type " + std::to_string(static_cast<int>(type)));
	}
}
