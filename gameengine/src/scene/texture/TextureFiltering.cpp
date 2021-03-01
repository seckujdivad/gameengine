#include "TextureFiltering.h"

#include <stdexcept>
#include <string>

TextureFiltering GetTextureFiltering(GLint filtering)
{
	switch (filtering)
	{
	case GL_NEAREST: return TextureFiltering::Nearest;
	case GL_LINEAR: return TextureFiltering::Linear;
	default: throw std::invalid_argument("Unknown texture filtering " + std::to_string(filtering));
	}
}

GLint GetTextureFilteringEnum(TextureFiltering filtering)
{
	switch (filtering)
	{
	case TextureFiltering::Nearest: return GL_NEAREST;
	case TextureFiltering::Linear: return GL_LINEAR;
	default: throw std::invalid_argument("Unknown texture filtering " + std::to_string(static_cast<int>(filtering)));
	}
}
