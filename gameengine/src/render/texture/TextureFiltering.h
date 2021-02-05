#pragma once

#include "../../GLComponents.h"

enum class TextureFiltering
{
	Nearest,
	Linear
};

TextureFiltering GetTextureFiltering(GLint filtering);
GLint GetTextureFilteringEnum(TextureFiltering filtering);