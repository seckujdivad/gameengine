#pragma once

#include "../GLComponents.h"

#include <vector>
#include <tuple>
#include <string>

#include "Renderable.h"

struct RenderTextureGroup
{
	GLuint colour = NULL;
	GLuint depth = NULL;
	std::vector<GLuint> data;
};

class RenderTexture : public Renderable
{
private:
	GLuint m_fbo;

	bool m_simultaneous_read_write;
	RenderTextureGroup m_texture_write;
	RenderTextureGroup m_texture_read;

	std::tuple<int, int> m_dimensions;

	int m_num_data_tex;

	void InitialiseTextureGroup(RenderTextureGroup& texture_group, int num_data_tex);
	void ResizeTextureGroup(RenderTextureGroup& texture_group);

	void PostRenderEvent() override;

public:
	RenderTexture(Scene* scene, std::vector<std::tuple<std::string, GLenum>> shaders, int num_data_tex, bool simultaneous_read_write = false);
	~RenderTexture();

	std::tuple<int, int> GetOutputSize() override;
	void SetOutputSize(std::tuple<int, int> dimensions);

	RenderTextureGroup GetOutputTextures();
};