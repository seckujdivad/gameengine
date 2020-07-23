#include "RenderTexture.h"

void RenderTexture::CreateTextureData(GLuint& texture, GLenum type, GLenum internal_format, GLenum format, std::tuple<int, int> dimensions, GLint filtering, bool do_create)
{
	glGenTextures(1, &texture);
	glBindTexture(type, texture);
	glTexParameteri(type, GL_TEXTURE_MAG_FILTER, filtering);
	glTexParameteri(type, GL_TEXTURE_MIN_FILTER, filtering);
	glTexParameteri(type, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(type, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(type, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	if (type == GL_TEXTURE_2D)
	{
		glTexImage2D(GL_TEXTURE_2D, 0, internal_format, std::get<0>(dimensions), std::get<1>(dimensions), 0, internal_format, format, NULL);
	}
	else if (type == GL_TEXTURE_CUBE_MAP)
	{
		for (int i = 0; i < 6; i++)
		{
			glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, internal_format, std::get<0>(dimensions), std::get<1>(dimensions), 0, internal_format, format, NULL);
		}
	}
	else
	{
		throw std::runtime_error("Unknown texture type " + std::to_string(type));
	}
}

void RenderTexture::InitialiseTextureGroup(RenderTextureGroup& texture_group, int num_data_tex, GLenum type, bool do_create)
{
	texture_group.dimensions = this->m_dimensions;

	this->CreateTextureData(texture_group.colour, type, GL_RGBA, GL_UNSIGNED_BYTE, this->m_dimensions, GL_LINEAR, do_create);
	this->CreateTextureData(texture_group.depth, type, GL_DEPTH_COMPONENT, GL_FLOAT, this->m_dimensions, GL_NEAREST, do_create);

	if (do_create)
	{
		texture_group.data.clear();
	}

	for (int i = 0; i < num_data_tex; i++)
	{
		GLuint texture_id;
		this->CreateTextureData(texture_id, type, GL_RGBA, GL_UNSIGNED_BYTE, this->m_dimensions, GL_NEAREST, do_create);
		texture_group.data.push_back(texture_id);
	}
}

void RenderTexture::ResizeTextureGroup(RenderTextureGroup& texture_group)
{
	glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);
	this->InitialiseTextureGroup(texture_group, texture_group.data.size(), false);
	glViewport(0, 0, std::get<0>(this->m_dimensions), std::get<1>(this->m_dimensions));
}

void RenderTexture::PostRenderEvent()
{
	if (this->m_simultaneous_read_write)
	{
		glCopyImageSubData(this->m_texture_write.colour, GL_TEXTURE_2D, 0, 0, 0, 0, this->m_texture_read.colour, GL_TEXTURE_2D, 0, 0, 0, 0, std::get<0>(this->m_dimensions), std::get<1>(this->m_dimensions), 1);
		glCopyImageSubData(this->m_texture_write.depth, GL_TEXTURE_2D, 0, 0, 0, 0, this->m_texture_read.depth, GL_TEXTURE_2D, 0, 0, 0, 0, std::get<0>(this->m_dimensions), std::get<1>(this->m_dimensions), 1);
		for (int i = 0; i < (int)this->m_texture_write.data.size(); i++)
		{
			glCopyImageSubData(this->m_texture_write.data.at(i), GL_TEXTURE_2D, 0, 0, 0, 0, this->m_texture_read.data.at(i), GL_TEXTURE_2D, 0, 0, 0, 0, std::get<0>(this->m_dimensions), std::get<1>(this->m_dimensions), 1);
		}
	}
}

RenderTexture::RenderTexture(RenderTextureReference reference, Engine* engine, std::vector<std::tuple<std::string, GLenum>> shaders, int num_data_tex, GLenum type, bool simultaneous_read_write) : Renderable(engine, shaders), Referenceable<RenderTextureReference>(reference)
{
	this->m_simultaneous_read_write = simultaneous_read_write;
	this->m_num_data_tex = num_data_tex;
	this->m_type = type;

	this->InitialiseTextureGroup(this->m_texture_write, num_data_tex, this->m_type);
	if (simultaneous_read_write)
	{
		this->InitialiseTextureGroup(this->m_texture_read, num_data_tex, this->m_type);
	}

	glGenFramebuffers(1, &this->m_fbo);
	glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, this->m_texture_write.type, this->m_texture_write.colour, 0);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, this->m_texture_write.type, this->m_texture_write.depth, 0);
	for (int i = 0; i < (int)this->m_texture_write.data.size(); i++)
	{
		glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1 + i, this->m_texture_write.type, this->m_texture_write.data.at(i), 0);
	}

	this->SetFramebuffer(this->m_fbo);
}

RenderTexture::~RenderTexture()
{
	glDeleteFramebuffers(1, &this->m_fbo);

	std::vector<GLuint> textures;
	
	textures.push_back(this->m_texture_write.colour);
	textures.push_back(this->m_texture_write.depth);
	textures.insert(textures.end(), this->m_texture_write.data.begin(), this->m_texture_write.data.end());

	if (this->m_simultaneous_read_write)
	{
		textures.push_back(this->m_texture_read.colour);
		textures.push_back(this->m_texture_read.depth);
		textures.insert(textures.end(), this->m_texture_read.data.begin(), this->m_texture_read.data.end());
	}

	glDeleteTextures(textures.size(), textures.data());
}

std::tuple<int, int> RenderTexture::GetOutputSize()
{
	return this->m_dimensions;
}

void RenderTexture::SetOutputSize(std::tuple<int, int> dimensions)
{
	if (dimensions != this->m_dimensions)
	{
		this->m_dimensions = dimensions;

		this->ResizeTextureGroup(this->m_texture_write);
		if (this->m_simultaneous_read_write)
		{
			this->ResizeTextureGroup(this->m_texture_read);
		}
	}
}

RenderTextureGroup RenderTexture::GetOutputTextures()
{
	if (this->m_simultaneous_read_write)
	{
		return this->m_texture_read;
	}
	else
	{
		return this->m_texture_write;
	}
}
