#include "RenderTexture.h"

#include <stdexcept>

#include "../../../GLComponents.h"

#include "../../../Engine.h"
#include "../../gltexture/GLTextureType.h"
#include "../../gltexture/GLTextureFormat.h"
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

RenderTexture::RenderTexture(RenderTextureReference reference, Engine* engine, RenderTargetConfig config, std::optional<std::shared_ptr<RenderTextureGroup>> write_textures, bool simultaneous_read_write, bool auto_swap_buffers)
	:
	RenderTarget(engine, config),
	Referenceable<RenderTextureReference>(reference),
	m_auto_swap_buffers(auto_swap_buffers)
{
	if (write_textures.has_value())
	{
		this->m_texture_write = write_textures.value();
		this->SetTargetType(this->m_texture_write.get()->GetTargetType());

		if (simultaneous_read_write)
		{
			this->m_texture_read = std::make_shared<RenderTextureGroup>();
			this->m_texture_write->CopyTo(**this->m_texture_read, true);
		}

		GLuint fbo;
		glGenFramebuffers(1, &fbo);
		this->SetFramebuffer(GLFramebuffer(fbo, true));
		this->GetWriteTextures()->AttachToFBO(fbo);
		this->GetFramebuffer().SetLabel("Render texture framebuffer: " + GetRenderTargetModeName(config.GetMode()));
	}
}

std::tuple<int, int> RenderTexture::GetOutputSize() const
{
	return this->m_texture_write.get()->GetDimensions();
}

bool RenderTexture::SetOutputSize(std::tuple<int, int> dimensions)
{
	bool changed_dimensions = this->m_texture_write.get()->SetDimensions(dimensions);
	if (this->m_texture_read.has_value())
	{
		changed_dimensions = this->m_texture_read->get()->SetDimensions(dimensions) || changed_dimensions;
	}
	return changed_dimensions;
}

void RenderTexture::SetWriteTarget(RenderTexture* target)
{
	this->SetFramebuffer(GLFramebuffer(target->GetFramebuffer().GetFBO(), false));
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
			src_tex->GetOutputTextures()->CopyTo(*this->GetWriteTextures().get(), true);
			this->SwapBuffers();
		}
	}
}
