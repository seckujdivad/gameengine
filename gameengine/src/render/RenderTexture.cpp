#include "RenderTexture.h"

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

void RenderTexture::InitialiseTextureGroup(RenderTextureGroup& texture_group, int num_data_tex, GLenum type, bool do_create)
{
	texture_group.dimensions = this->m_dimensions;
	texture_group.type = type;

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
	glBindFramebuffer(GL_FRAMEBUFFER, this->GetFramebuffer());
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

RenderTexture::RenderTexture(RenderTextureReference reference, Engine* engine, RenderMode mode, RenderTextureInfo info, GLenum type, bool simultaneous_read_write)
	:
	Renderable(engine, mode),
	Referenceable<RenderTextureReference>(reference),
	m_dimensions(1, 1),
	m_simultaneous_read_write(simultaneous_read_write),
	m_info(info),
	m_type(type)
{
	this->InitialiseTextureGroup(this->m_texture_write, ENGINECANVAS_NUM_DATA_TEX, this->m_type);
	if (simultaneous_read_write)
	{
		this->InitialiseTextureGroup(this->m_texture_read, ENGINECANVAS_NUM_DATA_TEX, this->m_type);
	}

	this->GetEngine()->MakeContextCurrent();

	GLuint fbo;
	glGenFramebuffers(1, &fbo);
	glBindFramebuffer(GL_FRAMEBUFFER, fbo);

	auto GenerateFramebufferTexture = [](GLenum attachment, GLenum textarget, GLuint& texture) {
		if (textarget == GL_TEXTURE_2D)
		{
			glFramebufferTexture2D(GL_FRAMEBUFFER, attachment, textarget, texture, 0);
		}
		else if (textarget == GL_TEXTURE_CUBE_MAP)
		{
			glFramebufferTexture(GL_FRAMEBUFFER, attachment, texture, 0);
		}
	};

	if (info.colour)
	{
		GenerateFramebufferTexture(GL_COLOR_ATTACHMENT0, this->m_texture_write.type, this->m_texture_write.colour);
	}

	if (info.depth)
	{
		GenerateFramebufferTexture(GL_DEPTH_ATTACHMENT, this->m_texture_write.type, this->m_texture_write.depth);
	}

	for (int i = 0; i < (int)this->m_texture_write.data.size(); i++)
	{
		GenerateFramebufferTexture(GL_COLOR_ATTACHMENT1 + i, this->m_texture_write.type, this->m_texture_write.data.at(i));
	}

	if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
	{
		throw std::runtime_error("Framebuffer is not complete: " + std::to_string((int)glGetError()) + std::string(" - ") + std::to_string(glCheckFramebufferStatus(GL_FRAMEBUFFER)));
	}

	this->SetFramebuffer(fbo);
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

RenderTextureInfo RenderTexture::GetTextureInfo()
{
	return this->m_info;
}
