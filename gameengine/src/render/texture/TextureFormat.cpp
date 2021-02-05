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
	default: throw std::invalid_argument("Unknown texture format " + std::to_string(static_cast<int>(format)));
	}
}
