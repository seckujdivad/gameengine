#include "RenderTextureGroup.h"

#include <stdexcept>

#include "../target/RenderTargetMode.h"
#include "../../TargetType.h"

const Texture& RenderTextureGroup::GetATexture() const
{
	if (this->depth.has_value())
	{
		return this->depth.value();
	}
	else if (this->colour.size() > 0)
	{
		return this->colour.at(0);
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
		|| (mode == RenderTargetMode::Normal_Draw)
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
				Texture::Preset preset;
				if (i == 0 && mode != RenderTargetMode::Normal_DepthOnly)
				{
					preset = Texture::Preset::Colour;
				}
				else
				{
					preset = Texture::Preset::Data;
				}
				this->colour.push_back(Texture(preset, target));
			}
		}
		else
		{
			throw std::invalid_argument("Can't generate RenderTextureGroup - mode should link to other textures");
		}

		if (mode != RenderTargetMode::PostProcess)
		{
			this->depth = Texture(Texture::Preset::Depth, target);
		}
	}
	else
	{
		throw std::invalid_argument("Can't generate RenderTextureGroup for mode " + std::to_string(static_cast<int>(mode)));
	}
}

std::tuple<int, int> RenderTextureGroup::GetDimensions() const
{
	return this->GetATexture().GetDimensions();
}

void RenderTextureGroup::SetDimensions(std::tuple<int, int> dimensions)
{
	if (this->depth.has_value())
	{
		this->depth->SetDimensions(dimensions);
	}

	for (Texture& texture : this->colour)
	{
		texture.SetDimensions(dimensions);
	}
}

TargetType RenderTextureGroup::GetTargetType() const
{
	return this->GetATexture().GetTargetType();
}

void RenderTextureGroup::CopyFrom(const RenderTextureGroup& src)
{
	src.CopyTo(*this);
}

void RenderTextureGroup::CopyTo(RenderTextureGroup& dest) const
{
	if (this->colour.size() != dest.colour.size())
	{
		throw std::runtime_error("The texture groups don't have the same number of colour textures");
	}

	for (int i = 0; i < static_cast<int>(this->colour.size()); i++)
	{
		this->colour.at(i).CopyTo(dest.colour.at(i));
	}

	if (this->depth.has_value() != dest.depth.has_value())
	{
		throw std::runtime_error("One texture group has a depth buffer, but the other doesn't");
	}

	if (this->depth.has_value())
	{
		this->depth->CopyTo(dest.depth.value());
	}
}

void RenderTextureGroup::ForEachTexture(std::function<void(Texture& texture)> foreach)
{
	for (Texture& texture : this->colour)
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
	for (const Texture& texture : this->colour)
	{
		result.colour.push_back(texture.GetTexture());
	}

	if (this->depth.has_value())
	{
		result.depth = this->depth->GetTexture();
	}
	
	return result;
}

void RenderTextureGroup::AttachToFBO(GLuint fbo) const
{
	glBindFramebuffer(GL_FRAMEBUFFER, fbo);

	std::vector<GLenum> attachments;
	for (int i = 0; i < static_cast<int>(this->colour.size()); i++)
	{
		const Texture& texture = this->colour.at(i);
		glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + i, texture.GetTexture(), 0);
		attachments.push_back(GL_COLOR_ATTACHMENT0 + i);
	}
	glDrawBuffers(static_cast<GLsizei>(attachments.size()), attachments.data());

	if (this->depth.has_value())
	{
		glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, this->depth.value().GetTexture(), 0);
	}

	if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
	{
		throw std::runtime_error("Framebuffer is not complete: " + GL_CHECK_ERROR() + " - " + std::to_string(glCheckFramebufferStatus(GL_FRAMEBUFFER)));
	}

	glBindFramebuffer(GL_FRAMEBUFFER, 0);
}
