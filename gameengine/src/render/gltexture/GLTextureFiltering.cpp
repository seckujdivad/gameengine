#include "GLTextureFiltering.h"

#include <stdexcept>
#include <string>

GLTextureFiltering GetTextureFiltering(GLint filtering)
{
	switch (filtering)
	{
	case GL_NEAREST: return GLTextureFiltering::Nearest;
	case GL_LINEAR: return GLTextureFiltering::Linear;
	default: throw std::invalid_argument("Unknown texture filtering " + std::to_string(filtering));
	}
}

GLint GetTextureFilteringEnum(GLTextureFiltering filtering)
{
	switch (filtering)
	{
	case GLTextureFiltering::Nearest: return GL_NEAREST;
	case GLTextureFiltering::Linear: return GL_LINEAR;
	default: throw std::invalid_argument("Unknown texture filtering " + std::to_string(static_cast<int>(filtering)));
	}
}
