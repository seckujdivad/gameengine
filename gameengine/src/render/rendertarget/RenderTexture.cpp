#include "RenderTexture.h"

#include <stdexcept>

#include "../../Engine.h"
#include "../texture/TextureType.h"
#include "../texture/TextureFormat.h"
#include "../TargetType.h"
#include "RenderTextureData.h"
#include "RenderTargetMode.h"

void RenderTexture::PostRenderEvent()
{
	if (this->m_auto_swap_buffers)
	{
		this->SwapBuffers();
	}
}

RenderTexture::RenderTexture(RenderTextureReference reference, Engine* engine, RenderTargetConfig config, std::optional<RenderTextureGroup*> write_textures, bool simultaneous_read_write, bool auto_swap_buffers)
	:
	RenderTarget(engine, config),
	Referenceable<RenderTextureReference>(reference),
	m_auto_swap_buffers(auto_swap_buffers),
	m_owns_fbo(write_textures.has_value())
{
	if (this->m_owns_fbo)
	{
		this->m_texture_write = std::make_shared<RenderTextureGroup>(std::move(*write_textures.value()));
		this->SetTargetType(this->m_texture_write.get()->GetTargetType());

		if (simultaneous_read_write)
		{
			this->m_texture_read = std::make_shared<RenderTextureGroup>(*this->m_texture_write.get());
		}

		GLuint fbo;
		glGenFramebuffers(1, &fbo);
		this->SetFramebuffer(fbo);
		this->AttachTexturesToFramebuffer();
	}
}

RenderTexture::~RenderTexture()
{
	if (this->m_owns_fbo)
	{
		GLuint fbo = this->GetFramebuffer();
		glDeleteFramebuffers(1, &fbo);
	}
}

void RenderTexture::AttachTexturesToFramebuffer()
{
	glBindFramebuffer(GL_FRAMEBUFFER, this->GetFramebuffer());

	std::vector<GLenum> attachments;

	for (int i = 0; i < static_cast<int>(this->m_texture_write.get()->colour.size()); i++)
	{
		Texture& texture = this->m_texture_write.get()->colour.at(i);
		glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + i, texture.GetTexture(), 0);
		attachments.push_back(GL_COLOR_ATTACHMENT0 + i);
	}

	if (this->m_texture_write.get()->depth.has_value())
	{
		glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, this->m_texture_write.get()->depth.value().GetTexture(), 0);
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
	return this->m_texture_write.get()->GetDimensions();
}

bool RenderTexture::SetOutputSize(std::tuple<int, int> dimensions)
{
	if (dimensions == this->m_texture_write.get()->GetDimensions())
	{
		return false;
	}
	else
	{
		this->m_texture_write.get()->SetDimensions(dimensions);
		if (this->m_texture_read.has_value())
		{
			this->m_texture_read->get()->SetDimensions(dimensions);
		}
		return true;
	}
}

void RenderTexture::SetWriteTarget(RenderTexture* target)
{
	if (this->m_owns_fbo)
	{
		GLuint fbo = this->GetFramebuffer();
		glDeleteFramebuffers(1, &fbo);
	}
	this->m_owns_fbo = false;

	this->SetFramebuffer(target->GetFramebuffer());
	this->SetTargetType(target->GetTargetType());

	this->m_texture_write = target->m_texture_write;
	this->m_texture_read = target->m_texture_read;
}

std::shared_ptr<RenderTextureGroup> RenderTexture::GetOutputTextures() const
{
	if (this->m_texture_read.has_value())
	{
		return this->m_texture_read.value();
	}
	else
	{
		return this->m_texture_write;
	}
}

std::shared_ptr<RenderTextureGroup> RenderTexture::GetWriteTextures() const
{
	return this->m_texture_write;
}

bool RenderTexture::SwapBuffers()
{
	if (this->m_texture_read.has_value())
	{
		this->m_texture_write->CopyTo(*this->m_texture_read.value());
	}
	return true;
}

bool RenderTexture::DoAutoSwapBuffers() const
{
	return this->m_auto_swap_buffers;
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

void RenderTexture::CopyFrom(const RenderTarget* src)
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
			src_tex->GetOutputTextures()->CopyTo(*this->GetWriteTextures().get());
			this->SwapBuffers();
		}
	}
}
