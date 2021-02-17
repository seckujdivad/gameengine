#include "TargetType.h"

#include <stdexcept>
#include <string>

TargetType GetTargetType(GLenum target)
{
	switch (target)
	{
	case GL_TEXTURE_2D: return TargetType::Texture_2D;
	case GL_TEXTURE_CUBE_MAP: return TargetType::Texture_Cubemap;
	default: throw std::invalid_argument("Unknown target: " + std::to_string(target));
	}
}

GLenum GetTargetEnum(TargetType target)
{
	switch (target)
	{
	case TargetType::Texture_2D: return GL_TEXTURE_2D;
	case TargetType::Texture_Cubemap: return GL_TEXTURE_CUBE_MAP;
	default: throw std::invalid_argument("Unknown target: " + std::to_string(static_cast<int>(target)));
	}
}

int GetNumTextures(TargetType target)
{
	switch (target)
	{
	case TargetType::Texture_2D: return 1;
	case TargetType::Texture_Cubemap: return 6;
	default: throw std::invalid_argument("Unknown target");
	}
}
