#include "RenderTextureGroup.h"

#include <stdexcept>

#include "../target/RenderTargetMode.h"
#include "../../TargetType.h"
#include "../../../scene/texture/TextureFiltering.h"

const GLTexture& RenderTextureGroup::GetATexture() const
{
	if (this->depth.has_value())
	{
		return *this->depth.value();
	}
	else if (this->colour.size() > 0)
	{
		return *this->colour.at(0);
	}
	else
	{
		throw std::runtime_error("This RenderTextureGroup contains no textures");
	}
}

RenderTextureGroup::RenderTextureGroup()
{
}

RenderTextureGroup::RenderTextureGroup(RenderTargetMode mode, TargetType target)
{
	if ((mode == RenderTargetMode::Normal_DepthOnly)
		|| (mode == RenderTargetMode::Normal_PostProcess)
		|| (mode == RenderTargetMode::Wireframe)
		|| (mode == RenderTargetMode::Textured)
		|| (mode == RenderTargetMode::PostProcess)
		|| (mode == RenderTargetMode::Shadow))
	{
		std::optional<int> num_colour_textures = GetNumColourTextures(mode);
		if (num_colour_textures.has_value())
		{
			for (int i = 0; i < num_colour_textures.value(); i++)
			{
				GLTexture::Preset preset;
				if (i == 0 && mode != RenderTargetMode::Normal_DepthOnly)
				{
					preset = GLTexture::Preset::Colour;
				}
				else
				{
					preset = GLTexture::Preset::Data_LowP;
				}

				int num_channels = 4;
				if (i == 1 && mode == RenderTargetMode::Normal_DepthOnly)
				{
					num_channels = 1;
				}

				this->colour.push_back(std::make_shared<GLTexture>(preset, target, num_channels));
			}
		}
		else
		{
			throw std::invalid_argument("Can't generate RenderTextureGroup - mode should link to other textures");
		}

		if (mode != RenderTargetMode::PostProcess)
		{
			this->depth = std::make_shared<GLTexture>(GLTexture::Preset::Depth, target);
		}
	}
	else if (mode == RenderTargetMode::Normal_Draw)
	{
		int num_colour_textures = GetNumAttachedColourTextures(mode);
		for (int i = 0; i < num_colour_textures; i++)
		{
			GLTexture::Preset preset;
			if (i == 0)
			{
				preset = GLTexture::Preset::Colour;
			}
			else if (i == 1)
			{
				preset = GLTexture::Preset::Data_MediumP;
			}
			else
			{
				preset = GLTexture::Preset::Data_LowP;
			}

			int num_channels = 4;
			if (i == 1)
			{
				num_channels = 2;
			}

			this->colour.push_back(std::make_shared<GLTexture>(preset, target, num_channels));
		}

		this->depth = std::make_shared<GLTexture>(GLTexture::Preset::Depth, target);
	}
	else if (mode == RenderTargetMode::Normal_SSRQuality)
	{
		this->colour.push_back(std::make_shared<GLTexture>(GLTexture::Preset::Data_LowP, target, GetNumAttachedColourTextures(RenderTargetMode::Normal_SSRQuality)));
	}
	else
	{
		throw std::invalid_argument("Can't generate RenderTextureGroup for mode " + GetRenderTargetModeName(mode));
	}

	//name textures
	for (std::size_t i = 0; i < this->colour.size(); i++)
	{
		this->colour.at(i)->SetLabel(GetRenderTargetModeName(mode) + ": colour render texture");
	}

	if (this->depth.has_value())
	{
		this->depth.value()->SetLabel(GetRenderTargetModeName(mode) + ": depth render texture");
	}
}

std::tuple<int, int> RenderTextureGroup::GetDimensions() const
{
	return this->GetATexture().GetDimensions();
}

bool RenderTextureGroup::SetDimensions(std::tuple<int, int> dimensions)
{
	bool changed_dimensions = false;
	if (this->depth.has_value())
	{
		changed_dimensions = this->depth.value()->SetDimensions(dimensions) || changed_dimensions;
	}

	for (std::shared_ptr<GLTexture>& texture : this->colour)
	{
		changed_dimensions = texture->SetDimensions(dimensions) || changed_dimensions;
	}
	return changed_dimensions;
}

TargetType RenderTextureGroup::GetTargetType() const
{
	return this->GetATexture().GetTargetType();
}

void RenderTextureGroup::CopyFrom(const RenderTextureGroup& src, bool deep_copy_textures)
{
	src.CopyTo(*this, deep_copy_textures);
}

void RenderTextureGroup::CopyTo(RenderTextureGroup& dest, bool deep_copy_textures) const
{
	if (this->colour.size() != dest.colour.size())
	{
		throw std::invalid_argument("The texture groups don't have the same number of colour textures");
	}

	if (deep_copy_textures)
	{
		for (int i = 0; i < static_cast<int>(this->colour.size()); i++)
		{
			this->colour.at(i)->CopyTo(*dest.colour.at(i));
		}
	}
	else
	{
		dest.colour = this->colour;
	}

	if (this->depth.has_value() != dest.depth.has_value())
	{
		if (dest.depth.has_value())
		{
			throw std::invalid_argument("The destination expects a depth texture, but the source doesn't have one");
		}
		else
		{
			throw std::invalid_argument("The source has a depth texture, but the destination doesn't expect one");
		}
	}

	if (deep_copy_textures)
	{
		if (this->depth.has_value())
		{
			this->depth.value()->CopyTo(*dest.depth.value());
		}
	}
	else
	{
		dest.depth = this->depth;
	}
}

void RenderTextureGroup::ForEachTexture(std::function<void(std::shared_ptr<GLTexture>& texture)> foreach)
{
	for (std::shared_ptr<GLTexture>& texture : this->colour)
	{
		foreach(texture);
	}
	
	if (this->depth.has_value())
	{
		foreach(this->depth.value());
	}
}

RenderTextureGroup::Identifiers RenderTextureGroup::GetIdentifiers() const
{
	Identifiers result = Identifiers();
	for (const std::shared_ptr<GLTexture>& texture : this->colour)
	{
		result.colour.push_back(texture->GetTexture());
	}

	if (this->depth.has_value())
	{
		result.depth = this->depth.value()->GetTexture();
	}
	
	return result;
}

void RenderTextureGroup::AttachToFBO(GLuint fbo) const
{
	glBindFramebuffer(GL_FRAMEBUFFER, fbo);

	std::vector<GLenum> attachments;
	for (int i = 0; i < static_cast<int>(this->colour.size()); i++)
	{
		const std::shared_ptr<GLTexture>& texture = this->colour.at(i);
		glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + i, texture->GetTexture(), 0);
		attachments.push_back(GL_COLOR_ATTACHMENT0 + i);
	}
	glDrawBuffers(static_cast<GLsizei>(attachments.size()), attachments.data());

	if (this->depth.has_value())
	{
		glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, this->depth.value()->GetTexture(), 0);
	}

	if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
	{
		throw std::runtime_error("Framebuffer is not complete: " + GL_CHECK_ERROR() + " - " + std::to_string(glCheckFramebufferStatus(GL_FRAMEBUFFER)));
	}

	glBindFramebuffer(GL_FRAMEBUFFER, 0);
}
