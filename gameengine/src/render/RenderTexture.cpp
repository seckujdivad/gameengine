#include "RenderTexture.h"

#include "../Engine.h"

void RenderTexture::CreateTextureData(GLuint& texture, GLenum type, GLenum internal_format, GLenum format, std::tuple<int, int> dimensions, GLint filtering, bool do_create)
{
	void* pixels = NULL;
#ifdef _DEBUG
	std::vector<GLubyte> pixel_data_ubyte(std::get<0>(dimensions) * std::get<1>(dimensions), 128);
	std::vector<GLfloat> pixel_data_float(std::get<0>(dimensions) * std::get<1>(dimensions), 0.5f);

	if (do_create)
	{
		if (format == GL_UNSIGNED_BYTE)
		{
			pixels = pixel_data_ubyte.data();
		}
		else if (format == GL_FLOAT)
		{
			pixels = pixel_data_float.data();
		}
		else
		{
			throw std::invalid_argument("Unknown format \"" + std::to_string(format) + "\"");
		}
	}
#endif

	if (do_create)
	{
		glGenTextures(1, &texture);
	}

	glBindTexture(type, texture);

	if (type == GL_TEXTURE_2D)
	{
		glTexImage2D(GL_TEXTURE_2D, 0, internal_format, std::get<0>(dimensions), std::get<1>(dimensions), 0, internal_format, format, pixels);
	}
	else if (type == GL_TEXTURE_CUBE_MAP)
	{
		for (int i = 0; i < 6; i++)
		{
			glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, internal_format, std::get<0>(dimensions), std::get<1>(dimensions), 0, internal_format, format, pixels);
		}
	}
	else
	{
		throw std::runtime_error("Unknown texture type " + std::to_string(type));
	}

	glTexParameteri(type, GL_TEXTURE_MAG_FILTER, filtering);
	glTexParameteri(type, GL_TEXTURE_MIN_FILTER, filtering);

	glTexParameteri(type, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(type, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(type, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	glTexParameteri(type, GL_TEXTURE_BASE_LEVEL, 0);
	glTexParameteri(type, GL_TEXTURE_MAX_LEVEL, 0);

#ifdef _DEBUG
	if (do_create)
	{
		bool arrays_are_different = false;

		if (format == GL_UNSIGNED_BYTE)
		{
			std::vector<GLubyte> return_data(std::get<0>(dimensions) * std::get<1>(dimensions));
			glGetTexImage(type == GL_TEXTURE_CUBE_MAP ? GL_TEXTURE_CUBE_MAP_POSITIVE_X : type, 0, internal_format, format, return_data.data());

			if (return_data.size() == pixel_data_ubyte.size())
			{
				for (std::size_t i = 0; i < return_data.size(); i++)
				{
					if (std::abs(return_data.at(i) - pixel_data_ubyte.at(i)) > 0)
					{
						arrays_are_different = true;
					}
				}
			}
			else
			{
				arrays_are_different = true;
			}
		}
		else if (format == GL_FLOAT)
		{
			std::vector<GLfloat> return_data(std::get<0>(dimensions) * std::get<1>(dimensions));
			glGetTexImage(type == GL_TEXTURE_CUBE_MAP ? GL_TEXTURE_CUBE_MAP_POSITIVE_X : type, 0, internal_format, format, return_data.data());
			
			if (return_data.size() == pixel_data_float.size())
			{
				for (std::size_t i = 0; i < return_data.size(); i++)
				{
					if (std::abs(return_data.at(i) - pixel_data_float.at(i)) > 0.000001f)
					{
						arrays_are_different = true;
					}
				}
			}
			else
			{
				arrays_are_different = true;
			}
		}
		else
		{
			throw std::invalid_argument("Unknown format \"" + std::to_string(format) + "\"");
		}

		if (arrays_are_different)
		{
			throw std::runtime_error("Texture is not using the set data");
		}
	}
#endif
}

void RenderTexture::InitialiseTextureGroup(RenderTextureGroup& texture_group, GLenum type, bool do_create)
{
	texture_group.dimensions = this->m_dimensions;
	texture_group.type = type;

	if (this->m_info.colour)
	{
		this->CreateTextureData(texture_group.colour, type, GL_RGBA, GL_UNSIGNED_BYTE, this->m_dimensions, this->m_info.colour_filtering, do_create);
	}

	if (this->m_info.depth)
	{
		this->CreateTextureData(texture_group.depth, type, GL_DEPTH_COMPONENT, GL_FLOAT, this->m_dimensions, this->m_info.depth_filtering, do_create);
	}

	if (do_create)
	{
		texture_group.data.clear();
	}

	for (int i = 0; i < this->m_info.num_data; i++)
	{
		GLuint texture_id;
		if (!do_create)
		{
			texture_id = texture_group.data.at(i);
		}

		this->CreateTextureData(texture_id, type, GL_RGBA, GL_UNSIGNED_BYTE, this->m_dimensions, this->m_info.data_filtering, do_create);

		if (do_create)
		{
			texture_group.data.push_back(texture_id);
		}
		else
		{
			texture_group.data.at(i) = texture_id;
		}
	}
}

void RenderTexture::ResizeTextureGroup(RenderTextureGroup& texture_group)
{
	glBindFramebuffer(GL_FRAMEBUFFER, this->GetFramebuffer());
	this->InitialiseTextureGroup(texture_group, texture_group.type, false);
	glViewport(0, 0, std::get<0>(this->m_dimensions), std::get<1>(this->m_dimensions));
}

void RenderTexture::PostRenderEvent()
{
	if (this->m_simultaneous_read_write)
	{
		if (this->m_info.colour)
		{
			glCopyImageSubData(this->m_texture_write.colour, this->m_texture_write.type, 0, 0, 0, 0, this->m_texture_read.colour, this->m_texture_write.type, 0, 0, 0, 0, std::get<0>(this->m_dimensions), std::get<1>(this->m_dimensions), this->m_texture_write.type == GL_TEXTURE_CUBE_MAP ? 6 : 1);
		}

		if (this->m_info.depth)
		{
			glCopyImageSubData(this->m_texture_write.depth, this->m_texture_write.type, 0, 0, 0, 0, this->m_texture_read.depth, this->m_texture_write.type, 0, 0, 0, 0, std::get<0>(this->m_dimensions), std::get<1>(this->m_dimensions), this->m_texture_write.type == GL_TEXTURE_CUBE_MAP ? 6 : 1);
		}

		for (int i = 0; i < (int)this->m_texture_write.data.size(); i++)
		{
			glCopyImageSubData(this->m_texture_write.data.at(i), this->m_texture_write.type, 0, 0, 0, 0, this->m_texture_read.data.at(i), this->m_texture_write.type, 0, 0, 0, 0, std::get<0>(this->m_dimensions), std::get<1>(this->m_dimensions), this->m_texture_write.type == GL_TEXTURE_CUBE_MAP ? 6 : 1);
		}
	}
}

RenderTexture::RenderTexture(RenderTextureReference reference, Engine* engine, RenderMode mode, RenderTextureInfo info, GLenum type, bool simultaneous_read_write)
	:
	Renderable(engine, mode),
	Referenceable<RenderTextureReference>(reference),
	m_dimensions(1, 1),
	m_simultaneous_read_write(simultaneous_read_write),
	m_info(info),
	m_type(type)
{
	this->GetEngine()->MakeContextCurrent();

	this->SetTargetType(type);

	GLuint fbo;
	glGenFramebuffers(1, &fbo);
	glBindFramebuffer(GL_DRAW_FRAMEBUFFER, fbo);
	this->SetFramebuffer(fbo);

	this->InitialiseTextureGroup(this->m_texture_write, this->m_type);
	if (simultaneous_read_write)
	{
		this->InitialiseTextureGroup(this->m_texture_read, this->m_type);
	}
	glBindTexture(type, NULL);

	std::vector<GLenum> attachments;

	if (info.colour)
	{
		glFramebufferTexture(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, this->m_texture_write.colour, 0);
		attachments.push_back(GL_COLOR_ATTACHMENT0);
	}

	if (info.depth)
	{
		glFramebufferTexture(GL_DRAW_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, this->m_texture_write.depth, 0);
	}

	for (int i = 0; i < (int)this->m_texture_write.data.size(); i++)
	{
		glFramebufferTexture(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT1 + i, this->m_texture_write.data.at(i), 0);
		attachments.push_back(GL_COLOR_ATTACHMENT1 + i);
	}

	glDrawBuffers(attachments.size(), attachments.data());
	glReadBuffer(GL_NONE);

	if (glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
	{
		throw std::runtime_error("Framebuffer is not complete: " + std::to_string((int)glGetError()) + " - " + std::to_string(glCheckFramebufferStatus(GL_FRAMEBUFFER)));
	}

	glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
}

RenderTexture::~RenderTexture()
{
	GLuint fbo = this->GetFramebuffer();
	glDeleteFramebuffers(1, &fbo);

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

std::tuple<int, int> RenderTexture::GetOutputSize() const
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

RenderTextureGroup RenderTexture::GetOutputTextures() const
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

RenderTextureInfo RenderTexture::GetTextureInfo() const
{
	return this->m_info;
}
