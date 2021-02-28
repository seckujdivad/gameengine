#include "RenderTexture.h"

#include <stdexcept>

#include "../../../GLComponents.h"

#include "../../../Engine.h"
#include "../../texture/TextureType.h"
#include "../../texture/TextureFormat.h"
#include "../../TargetType.h"
#include "RenderTextureGroup.h"
#include "../target/RenderTargetMode.h"

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
		this->GetWriteTextures()->AttachToFBO(fbo);
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
