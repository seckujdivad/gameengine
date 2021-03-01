#include "GLTextureFormat.h"

#include <stdexcept>
#include <string>

GLTextureFormat GetTextureFormat(GLint format)
{
	switch (format)
	{
	case GL_DEPTH_COMPONENT: return GLTextureFormat::Depth;
	case GL_RED: return GLTextureFormat::R;
	case GL_RG: return GLTextureFormat::RG;
	case GL_RGB: return GLTextureFormat::RGB;
	case GL_RGBA: return GLTextureFormat::RGBA;
	case GL_RGB8: return GLTextureFormat::RGB8;
	case GL_RGBA16F: return GLTextureFormat::RGBA16F;
	case GL_RGBA32F: return GLTextureFormat::RGBA32F;
	default: throw std::invalid_argument("Unknown texture format " + std::to_string(format));
	}
}

GLint GetTextureFormatEnum(GLTextureFormat format)
{
	switch (format)
	{
	case GLTextureFormat::Depth: return GL_DEPTH_COMPONENT;
	case GLTextureFormat::R: return GL_RED;
	case GLTextureFormat::RG: return GL_RG;
	case GLTextureFormat::RGB: return GL_RGB;
	case GLTextureFormat::RGBA: return GL_RGBA;
	case GLTextureFormat::RGB8: return GL_RGB8;
	case GLTextureFormat::RGBA16F: return GL_RGBA16F;
	case GLTextureFormat::RGBA32F: return GL_RGBA32F;
	default: throw std::invalid_argument("Unknown texture format " + std::to_string(static_cast<int>(format)));
	}
}
