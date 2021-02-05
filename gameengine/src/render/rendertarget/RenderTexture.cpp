#include "RenderTexture.h"

#include <stdexcept>

#include "../../Engine.h"
#include "../texture/TextureType.h"
#include "../texture/TextureFormat.h"

void RenderTexture::CreateTextureData(GLuint& texture, GLenum target, TextureFormat format, TextureType type, std::tuple<int, int> dimensions, GLint filtering, bool do_create)
{
	const auto& [size_x, size_y] = dimensions;

	GLint internal_format = GL_NONE;
	switch (format)
	{
	case TextureFormat::Depth: internal_format = GL_DEPTH_COMPONENT; break;
	case TextureFormat::R: internal_format = GL_RED; break;
	case TextureFormat::RG: internal_format = GL_RG; break;
	case TextureFormat::RGB: internal_format = GL_RGB; break;
	case TextureFormat::RGBA: internal_format = GL_RGBA; break;
	default: throw std::invalid_argument("Unknown texture format");
	}

	GLenum tex_type = GL_NONE;
	switch (type)
	{
	case TextureType::Float: tex_type = GL_FLOAT; break;
	case TextureType::HalfFloat: tex_type = GL_HALF_FLOAT; break;
	case TextureType::UnsignedByte: tex_type = GL_UNSIGNED_BYTE; break;
	default: throw std::invalid_argument("Unknown texture type");
	}

	GLint preferred_format = GL_NONE;
	glGetInternalformativ(target, internal_format, GL_TEXTURE_IMAGE_FORMAT, 1, &preferred_format);

	if (do_create)
	{
		glGenTextures(1, &texture);
	}
	glBindTexture(target, texture);

	if (target == GL_TEXTURE_2D)
	{
		glTexImage2D(GL_TEXTURE_2D, 0, internal_format, size_x, size_y, 0, static_cast<GLenum>(preferred_format), tex_type, nullptr);
	}
	else if (target == GL_TEXTURE_CUBE_MAP)
	{
		for (int i = 0; i < 6; i++)
		{
			glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, internal_format, size_x, size_y, 0, static_cast<GLenum>(preferred_format), tex_type, nullptr);
		}
	}
	else
	{
		throw std::invalid_argument("Unknown texture target " + std::to_string(target));
	}

	glTexParameteri(target, GL_TEXTURE_MAG_FILTER, filtering);
	glTexParameteri(target, GL_TEXTURE_MIN_FILTER, filtering);

	if (do_create) //these parameters don't change
	{
		glTexParameteri(target, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
		glTexParameteri(target, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
		glTexParameteri(target, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

		glTexParameteri(target, GL_TEXTURE_BASE_LEVEL, 0);
		glTexParameteri(target, GL_TEXTURE_MAX_LEVEL, 0);
		glGenerateMipmap(target);
	}
}

void RenderTexture::InitialiseTextureGroup(RenderTextureGroup& texture_group, GLenum type, bool do_create)
{
	texture_group.dimensions = this->m_dimensions;
	texture_group.type = type;

	if (this->m_info.colour)
	{
		this->CreateTextureData(texture_group.colour, type, TextureFormat::RGBA, TextureType::UnsignedByte, this->m_dimensions, this->m_info.colour_filtering, do_create);
	}

	if (this->m_info.depth)
	{
		this->CreateTextureData(texture_group.depth, type, TextureFormat::Depth, TextureType::Float, this->m_dimensions, this->m_info.depth_filtering, do_create);
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

		this->CreateTextureData(texture_id, type, TextureFormat::RGBA, TextureType::HalfFloat, this->m_dimensions, this->m_info.data_filtering, do_create);

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
	this->InitialiseTextureGroup(texture_group, texture_group.type, false);
}

bool RenderTexture::CheckTextureGroup(RenderTextureGroup texture_group) const
{
	return ::CheckTextureGroup(texture_group, this->m_info);
}

void RenderTexture::PostRenderEvent()
{
	if (this->m_auto_swap_buffers)
	{
		this->SwapBuffers();
	}
}

RenderTexture::RenderTexture(RenderTextureReference reference, Engine* engine, RenderTargetConfig config, RenderTextureInfo info, GLenum type, bool simultaneous_read_write, bool auto_swap_buffers)
	:
	RenderTarget(engine, config),
	Referenceable<RenderTextureReference>(reference),
	m_dimensions(1, 1),
	m_simultaneous_read_write(simultaneous_read_write),
	m_info(info),
	m_type(type),
	m_auto_swap_buffers(auto_swap_buffers),
	m_owns_target(info.auto_generate_textures)
{
	this->SetTargetType(type);

	if (info.auto_generate_textures)
	{
		this->InitialiseTextureGroup(this->m_texture_write, this->m_type);
		if (simultaneous_read_write)
		{
			this->InitialiseTextureGroup(this->m_texture_read, this->m_type);
		}

		GLuint fbo;
		glGenFramebuffers(1, &fbo);
		this->SetFramebuffer(fbo);
		this->AttachTexturesToFramebuffer();
	}
}

RenderTexture::~RenderTexture()
{
	if (this->m_owns_target)
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

		glDeleteTextures(static_cast<GLsizei>(textures.size()), textures.data());
	}
}

void RenderTexture::AttachTexturesToFramebuffer()
{
	glBindFramebuffer(GL_FRAMEBUFFER, this->GetFramebuffer());

	std::vector<GLenum> attachments;

	if (this->m_info.colour)
	{
		glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, this->m_texture_write.colour, 0);
		attachments.push_back(GL_COLOR_ATTACHMENT0);
	}

	if (this->m_info.depth)
	{
		glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, this->m_texture_write.depth, 0);
	}

	for (int i = 0; i < static_cast<int>(this->m_texture_write.data.size()); i++)
	{
		glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1 + i, this->m_texture_write.data.at(i), 0);
		attachments.push_back(GL_COLOR_ATTACHMENT1 + i);
	}

	glDrawBuffers(static_cast<GLsizei>(attachments.size()), attachments.data());

	if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
	{
		throw std::runtime_error("Framebuffer is not complete: " + GL_CHECK_ERROR() + " - " + std::to_string(glCheckFramebufferStatus(GL_FRAMEBUFFER)));
	}

	glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

std::tuple<int, int> RenderTexture::GetOutputSize() const
{
	return this->m_dimensions;
}

bool RenderTexture::SetOutputSize(std::tuple<int, int> dimensions)
{
	if (dimensions == this->m_dimensions)
	{
		return false;
	}
	else
	{
		this->m_dimensions = dimensions;

		this->ResizeTextureGroup(this->m_texture_write);
		if (this->m_simultaneous_read_write)
		{
			this->ResizeTextureGroup(this->m_texture_read);
		}

		return true;
	}
}

void RenderTexture::SetWriteTarget(RenderTexture* target)
{
	this->SetFBO(target->GetFramebuffer());

	RenderTextureGroup write_textures = target->GetWriteTextures();
	if (this->m_info.colour)
	{
		if (write_textures.colour == NULL)
		{
			throw std::invalid_argument("Target must have a colour texture");
		}
	}
	else
	{
		write_textures.colour = NULL;
	}

	if (this->m_info.depth)
	{
		if (write_textures.depth == NULL)
		{
			throw std::invalid_argument("Target must have a depth texture");
		}
	}
	else
	{
		write_textures.depth = NULL;
	}

	if (this->m_info.num_data > static_cast<int>(write_textures.data.size()))
	{
		throw std::invalid_argument("Target must have " + std::to_string(this->m_info.num_data) + " data texture(s)");
	}
	else if (this->m_info.num_data < static_cast<int>(write_textures.data.size()))
	{
		std::vector<GLuint> data_tex;
		data_tex.reserve(this->m_info.num_data);
		for (int i = 0; i < this->m_info.num_data; i++)
		{
			data_tex.push_back(write_textures.data.at(i));
		}
		write_textures.data = data_tex;
	}

	this->SetWriteTextures(write_textures, false);
}

void RenderTexture::SetFBO(GLuint fbo)
{
	if (this->m_owns_target)
	{
		throw std::runtime_error("RenderTexture must not own the target");
	}
	else
	{
		this->SetFramebuffer(fbo);
	}
}

void RenderTexture::SetWriteTextures(RenderTextureGroup textures, bool attach)
{
	if (this->m_owns_target)
	{
		throw std::runtime_error("RenderTexture must not own the target");
	}
	else if (this->CheckTextureGroup(textures))
	{
		this->m_texture_write = textures;

		if (attach)
		{
			this->AttachTexturesToFramebuffer();
		}
	}
	else
	{
		throw std::invalid_argument("Provided RenderTextureGroup must match the RenderTextureInfo of the RenderTexture");
	}
}

void RenderTexture::SetOutputTextures(RenderTextureGroup textures)
{
	if (this->m_owns_target)
	{
		throw std::runtime_error("RenderTexture must not own the target");
	}
	else if (this->m_simultaneous_read_write)
	{
		if (this->CheckTextureGroup(textures))
		{
			this->m_texture_read = textures;
		}
		else
		{
			throw std::invalid_argument("Provided RenderTextureGroup must match the RenderTextureInfo of the RenderTexture");
		}
	}
	else
	{
		this->SetWriteTextures(textures);
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

RenderTextureGroup RenderTexture::GetWriteTextures() const
{
	return this->m_texture_write;
}

RenderTextureInfo RenderTexture::GetTextureInfo() const
{
	return this->m_info;
}

bool RenderTexture::SwapBuffers()
{
	if (this->m_simultaneous_read_write)
	{
		CopyTextureGroup(this->m_texture_write, this->m_texture_read, this->m_info, this->m_dimensions);
	}
	return true;
}

void RenderTexture::SetNormalModePreviousFrameToSelf()
{
	if (this->GetRenderMode() == RenderTargetMode::Normal_Draw)
	{
		std::get<RenderTargetConfig::Normal_Draw>(this->m_config.mode_data).previous_frame = this->GetOutputTextures();
	}
	else
	{
		throw std::runtime_error("Render mode must be \"Normal_Draw\", not " + std::to_string(static_cast<int>(this->GetRenderMode())));
	}
}

void RenderTexture::CopyFrom(const RenderTarget* src) const
{
	if (this != src)
	{
		const RenderTexture* src_tex = dynamic_cast<const RenderTexture*>(src);

		if (src_tex == nullptr)
		{
			this->RenderTarget::CopyFrom(src);
		}
		else
		{
			CopyTextureGroup(src_tex->GetOutputTextures(), this->GetWriteTextures(), this->GetTextureInfo(), this->GetOutputSize());
		}
	}
}
