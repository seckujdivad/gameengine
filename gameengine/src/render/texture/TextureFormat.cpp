#include "TextureFormat.h"

#include <stdexcept>
#include <string>

TextureFormat GetTextureFormat(GLint format)
{
	switch (format)
	{
	case GL_DEPTH_COMPONENT: return TextureFormat::Depth;
	case GL_RED: return TextureFormat::R;
	case GL_RG: return TextureFormat::RG;
	case GL_RGB: return TextureFormat::RGB;
	case GL_RGBA: return TextureFormat::RGBA;
	case GL_RGB8: return TextureFormat::RGB8;
	case GL_RGBA16F: return TextureFormat::RGBA16F;
	case GL_RGBA32F: return TextureFormat::RGBA32F;
	default: throw std::invalid_argument("Unknown texture format " + std::to_string(format));
	}
}

GLint GetTextureFormatEnum(TextureFormat format)
{
	switch (format)
	{
	case TextureFormat::Depth: return GL_DEPTH_COMPONENT;
	case TextureFormat::R: return GL_RED;
	case TextureFormat::RG: return GL_RG;
	case TextureFormat::RGB: return GL_RGB;
	case TextureFormat::RGBA: return GL_RGBA;
	case TextureFormat::RGB8: return GL_RGB8;
	case TextureFormat::RGBA16F: return GL_RGBA16F;
	case TextureFormat::RGBA32F: return GL_RGBA32F;
	default: throw std::invalid_argument("Unknown texture format " + std::to_string(static_cast<int>(format)));
	}
}
