#pragma once

#include <string>

#include "../GLComponents.h"

struct LoadedTexture
{
	GLuint id = NULL;
	GLenum type = NULL;
	std::string uniform_name;
};