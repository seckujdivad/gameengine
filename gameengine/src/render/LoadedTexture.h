#pragma once

#include <string>
#include <memory>

#include "../GLComponents.h"

#include "texture/Texture.h"
#include "TargetType.h"

struct LoadedTexture
{
	GLuint id = NULL;
	GLenum type = NULL;
	std::string uniform_name;
};