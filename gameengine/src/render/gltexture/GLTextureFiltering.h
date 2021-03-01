#pragma once

#include "../../GLComponents.h"

enum class GLTextureFiltering
{
	Nearest,
	Linear
};

GLTextureFiltering GetTextureFiltering(GLint filtering);
GLint GetTextureFilteringEnum(GLTextureFiltering filtering);