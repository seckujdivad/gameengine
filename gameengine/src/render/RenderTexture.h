#pragma once

#include "../GLComponents.h"

#include <vector>
#include <tuple>
#include <string>

#include "Renderable.h"
#include "../scene/Referenceable.h"
#include "../Engine.h"

struct RenderTextureGroup
{
	GLenum type = GL_TEXTURE_2D;
	GLuint colour = NULL;
	GLuint depth = NULL;
	std::vector<GLuint> data;

	std::tuple<int, int> dimensions;
};

class RenderTexture : public Renderable, public Referenceable<RenderTextureReference>
{
private:
	GLuint m_fbo;
	GLenum m_type;

	bool m_simultaneous_read_write;
	RenderTextureGroup m_texture_write;
	RenderTextureGroup m_texture_read;

	std::tuple<int, int> m_dimensions;

	int m_num_data_tex;

	static void CreateTextureData(GLuint& texture, GLenum type, GLenum internal_format, GLenum format, std::tuple<int, int> dimensions, GLint filtering, bool do_create = true);

	void InitialiseTextureGroup(RenderTextureGroup& texture_group, int num_data_tex, GLenum type, bool do_create = true);
	void ResizeTextureGroup(RenderTextureGroup& texture_group);

	void PostRenderEvent() override;

public:
	RenderTexture(RenderTextureReference reference, Engine* engine, RenderMode mode, int num_data_tex, GLenum type = GL_TEXTURE_2D, bool simultaneous_read_write = false);
	~RenderTexture();

	std::tuple<int, int> GetOutputSize() override;
	void SetOutputSize(std::tuple<int, int> dimensions);

	RenderTextureGroup GetOutputTextures();
};
