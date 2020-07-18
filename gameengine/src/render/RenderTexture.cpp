#include "RenderTexture.h"

void RenderTexture::InitialiseTextureGroup(RenderTextureGroup& texture_group, int num_data_tex)
{
	glGenTextures(1, &texture_group.colour);
	glBindTexture(GL_TEXTURE_2D, texture_group.colour);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, std::get<0>(this->m_dimensions), std::get<1>(this->m_dimensions), 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	glGenTextures(1, &texture_group.depth);
	glBindTexture(GL_TEXTURE_2D, texture_group.depth);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, std::get<0>(this->m_dimensions), std::get<1>(this->m_dimensions), 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	for (int i = 0; i < num_data_tex; i++)
	{
		GLuint texture_id;
		glGenTextures(1, &texture_id);
		glBindTexture(GL_TEXTURE_2D, texture_id);
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, std::get<0>(this->m_dimensions), std::get<1>(this->m_dimensions), 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

		texture_group.data.push_back(texture_id);
	}
}

void RenderTexture::ResizeTextureGroup(RenderTextureGroup& texture_group)
{
	glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);
	
	glBindTexture(GL_TEXTURE_2D, texture_group.colour);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, std::get<0>(this->m_dimensions), std::get<1>(this->m_dimensions), 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);

	glBindTexture(GL_TEXTURE_2D, texture_group.depth);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, std::get<0>(this->m_dimensions), std::get<1>(this->m_dimensions), 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);

	for (int i = 0; i < (int)texture_group.data.size(); i++)
	{
		glBindTexture(GL_TEXTURE_2D, texture_group.data.at(i));
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, std::get<0>(this->m_dimensions), std::get<1>(this->m_dimensions), 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
	}

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

RenderTexture::RenderTexture(Scene* scene, std::vector<std::tuple<std::string, GLenum>> shaders, int num_data_tex, bool simultaneous_read_write) : Renderable(scene, shaders)
{
	this->m_simultaneous_read_write = simultaneous_read_write;
	this->m_num_data_tex = num_data_tex;

	this->InitialiseTextureGroup(this->m_texture_write, num_data_tex);
	if (simultaneous_read_write)
	{
		this->InitialiseTextureGroup(this->m_texture_read, num_data_tex);
	}

	glGenFramebuffers(1, &this->m_fbo);
	glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, this->m_texture_write.colour, 0);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, this->m_texture_write.depth, 0);
	for (int i = 0; i < (int)this->m_texture_write.data.size(); i++)
	{
		glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1 + i, GL_TEXTURE_2D, this->m_texture_write.data.at(i), 0);
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
