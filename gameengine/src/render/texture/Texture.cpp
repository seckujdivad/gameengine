#include "Texture.h"

#include <stdexcept>
#include <string>

#include "TextureType.h"
#include "TextureFormat.h"
#include "TextureFiltering.h"
#include "../TargetType.h"

void Texture::ConfigureTexture(bool create, std::optional<TextureFormat> pixel_format, std::vector<const void*> pixels)
{
	if (pixels.size() == 0) //fill with nullptr if default value provided
	{
		pixels.reserve(6);
		for (int i = 0; i < 6; i++)
		{
			pixels.push_back(nullptr);
		}
	}

	const auto& [size_x, size_y] = this->GetDimensions();

	GLenum tex_type = GetTextureTypeEnum(this->GetTextureType());
	GLint internal_format = GetTextureFormatEnum(this->GetFormat());
	GLint filtering = GetTextureFilteringEnum(this->GetFiltering());
	GLenum target = GetTargetEnum(this->GetTargetType());

	GLint preferred_format;
	if (pixel_format.has_value())
	{
		preferred_format = GetTextureFormatEnum(pixel_format.value());
	}
	else
	{
		preferred_format = this->GetPreferredFormat();
	}

	if (create)
	{
		glGenTextures(1, &this->m_texture);
	}
	glBindTexture(target, this->m_texture);

	if (target == GL_TEXTURE_2D)
	{
		glTexImage2D(GL_TEXTURE_2D, 0, internal_format, size_x, size_y, 0, static_cast<GLenum>(preferred_format), tex_type, pixels.at(0));
	}
	else if (target == GL_TEXTURE_CUBE_MAP)
	{
		for (int i = 0; i < 6; i++)
		{
			glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, internal_format, size_x, size_y, 0, static_cast<GLenum>(preferred_format), tex_type, pixels.at(i));
		}
	}

	glTexParameteri(target, GL_TEXTURE_MAG_FILTER, filtering);
	glTexParameteri(target, GL_TEXTURE_MIN_FILTER, filtering);

	if (create) //these parameters don't change
	{
		glTexParameteri(target, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
		glTexParameteri(target, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
		glTexParameteri(target, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

		if (!this->GetGenerateMipMaps())
		{
			glTexParameteri(target, GL_TEXTURE_BASE_LEVEL, 0);
			glTexParameteri(target, GL_TEXTURE_MAX_LEVEL, 0);
			
		}
		else
		{
			glGenerateMipmap(target);
		}
	}
}

GLint Texture::GetPreferredFormat(bool force)
{
	if (force || (this->m_preferred_format != GL_NONE))
	{
		glGetInternalformativ(GetTargetEnum(this->GetTargetType()), GetTextureFormatEnum(this->GetFormat()), GL_TEXTURE_IMAGE_FORMAT, 1, &this->m_preferred_format);
	}
	return this->m_preferred_format;
}

Texture::Texture(Preset preset, TargetType target, std::tuple<int, int> dimensions, bool generate_mipmaps) : m_dimensions(dimensions), m_target(target), m_generate_mipmaps(generate_mipmaps)
{
	if (preset == Preset::Colour)
	{
		this->m_type = TextureType::UnsignedByte;
		this->m_format = TextureFormat::RGBA;
		this->m_filtering = TextureFiltering::Linear;
	}
	else if (preset == Preset::Data)
	{
		this->m_type = TextureType::HalfFloat;
		this->m_format = TextureFormat::RGBA;
		this->m_filtering = TextureFiltering::Nearest;
	}
	else if (preset == Preset::Depth)
	{
		this->m_type = TextureType::Float;
		this->m_format = TextureFormat::Depth;
		this->m_filtering = TextureFiltering::Nearest;
	}
	else
	{
		throw std::invalid_argument("Unknown preset " + std::to_string(static_cast<int>(preset)));
	}

	this->ConfigureTexture(true);
}

Texture::Texture(Texture&& move_from) noexcept
{
	*this = std::move(move_from);
}

Texture& Texture::operator=(Texture&& move_from) noexcept
{
	this->m_dimensions = move_from.m_dimensions;
	this->m_type = move_from.m_type;
	this->m_format = move_from.m_format;
	this->m_filtering = move_from.m_filtering;
	this->m_target = move_from.m_target;
	this->m_generate_mipmaps = move_from.m_generate_mipmaps;

	this->m_preferred_format = move_from.m_preferred_format;

	this->m_texture = move_from.m_texture;
	move_from.m_texture = GL_NONE;

	return *this;
}

Texture::~Texture()
{
	if (this->m_texture != GL_NONE)
	{
		glDeleteTextures(1, &this->m_texture);
	}
}

void Texture::SetDimensions(std::tuple<int, int> dimensions)
{
	if (dimensions != this->m_dimensions)
	{
		this->m_dimensions = dimensions;
		this->ConfigureTexture(false);
	}
}

std::tuple<int, int> Texture::GetDimensions() const
{
	return this->m_dimensions;
}

void Texture::SetTextureType(TextureType type)
{
	if (type != this->m_type)
	{
		this->m_type = type;
		this->ConfigureTexture(false);
	}
}

TextureType Texture::GetTextureType() const
{
	return this->m_type;
}

void Texture::SetFormat(TextureFormat format)
{
	if (format != this->m_format)
	{
		this->m_format = format;
		this->GetPreferredFormat(true);
		this->ConfigureTexture(false);
	}
}

TextureFormat Texture::GetFormat() const
{
	return this->m_format;
}

void Texture::SetFiltering(TextureFiltering filtering)
{
	if (filtering != this->m_filtering)
	{
		this->m_filtering = filtering;
		this->ConfigureTexture(false);
	}
}

TextureFiltering Texture::GetFiltering() const
{
	return this->m_filtering;
}

void Texture::SetTargetType(TargetType target)
{
	if (target != this->m_target)
	{
		this->m_target = target;
		this->GetPreferredFormat(true);
		this->ConfigureTexture(false);
	}
}

TargetType Texture::GetTargetType() const
{
	return this->m_target;
}

void Texture::SetGenerateMipMaps(bool generate_mipmaps)
{
	if (generate_mipmaps != this->m_generate_mipmaps)
	{
		this->m_generate_mipmaps = generate_mipmaps;
		this->ConfigureTexture(false);
	}
}

bool Texture::GetGenerateMipMaps() const
{
	return false;
}

GLuint Texture::GetTexture() const
{
	return this->m_texture;
}

void Texture::BindTexture() const
{
	glBindTexture(GetTargetEnum(this->GetTargetType()), this->GetTexture());
}

void Texture::SetPixels(TextureFormat pixel_format, std::vector<const void*> pixels)
{
	this->ConfigureTexture(false, pixel_format, pixels);
}
