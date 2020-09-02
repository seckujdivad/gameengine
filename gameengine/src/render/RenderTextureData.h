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
	GLenum colour_filtering = GL_LINEAR;
	bool depth = true;
	GLenum depth_filtering = GL_NEAREST;
	int num_data = GAMEENGINE_NUM_DATA_TEX;
	GLenum data_filtering = GL_NEAREST;
};

void CopyTextureGroup(RenderTextureGroup source, RenderTextureGroup destination, RenderTextureInfo info, std::tuple<int, int> dimensions);