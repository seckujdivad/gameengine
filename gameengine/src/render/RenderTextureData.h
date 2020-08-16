#pragma once

#include <tuple>
#include <vector>

#include "../GLComponents.h"

struct RenderTextureGroup
{
	GLenum type = GL_TEXTURE_2D;
	GLuint colour = NULL;
	GLuint depth = NULL;
	std::vector<GLuint> data;

	std::tuple<int, int> dimensions;
};

struct RenderTextureInfo
{
	bool colour = true;
	bool depth = true;
};