#include "GLTextureType.h"

#include <stdexcept>
#include <string>

GLTextureType GetTextureType(GLenum type)
{
	switch (type)
	{
	case GL_FLOAT: return GLTextureType::Float;
	case GL_HALF_FLOAT: return GLTextureType::HalfFloat;
	case GL_UNSIGNED_BYTE: return GLTextureType::UnsignedByte;
	case GL_INT: return GLTextureType::Integer;
	default: throw std::invalid_argument("Unknown texture type " + std::to_string(type));
	}
}

GLenum GetTextureTypeEnum(GLTextureType type)
{
	switch (type)
	{
	case GLTextureType::Float: return GL_FLOAT;
	case GLTextureType::HalfFloat: return GL_HALF_FLOAT;
	case GLTextureType::UnsignedByte: return GL_UNSIGNED_BYTE;
	case GLTextureType::Integer: return GL_INT;
	default: throw std::invalid_argument("Unknown texture type " + std::to_string(static_cast<int>(type)));
	}
}
