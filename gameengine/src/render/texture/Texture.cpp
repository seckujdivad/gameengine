#include "Texture.h"

#include <stdexcept>
#include <string>

#include "TextureType.h"
#include "TextureFormat.h"
#include "TextureFiltering.h"
#include "../TargetType.h"

void Texture::ConfigureTexture(bool create, std::optional<TextureFormat> pixel_format, std::vector<const void*> pixels)
{
	//pad pixel data with nullptr
	std::size_t min_num_pixel_arrays = 0;
	switch (this->GetTargetType())
	{
	case (TargetType::Texture_2D): min_num_pixel_arrays = 1; break;
	case (TargetType::Texture_Cubemap): min_num_pixel_arrays = 6; break;
	default: throw std::invalid_argument("Unrecognised target type");
	}

	for (std::size_t i = pixels.size(); i < min_num_pixel_arrays; i++)
	{
		pixels.push_back(nullptr);
	}

	bool pixel_data_provided = false;
	for (const void* ptr : pixels)
	{
		if (ptr != nullptr)
		{
			pixel_data_provided = true;
		}
	}

	const auto& [size_x, size_y] = this->GetDimensions();

	GLenum tex_type = GetTextureTypeEnum(this->GetTextureType());
	GLint internal_format = GetTextureFormatEnum(this->GetFormat());
	GLint min_filtering = GetTextureFilteringEnum(this->GetMinFiltering());
	GLint mag_filtering = GetTextureFilteringEnum(this->GetMagFiltering());
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

	glTexParameteri(target, GL_TEXTURE_MAG_FILTER, mag_filtering);
	glTexParameteri(target, GL_TEXTURE_MIN_FILTER, min_filtering);

	if (create) //these parameters don't change
	{
		glTexParameteri(target, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
		glTexParameteri(target, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
		glTexParameteri(target, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
	}

	if (create || pixel_data_provided)
	{
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
	if (force || (this->m_preferred_format == GL_NONE))
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
		this->m_filtering_min = TextureFiltering::Linear;
		this->m_filtering_mag = TextureFiltering::Linear;
	}
	else if (preset == Preset::Data)
	{
		this->m_type = TextureType::HalfFloat;
		this->m_format = TextureFormat::RGBA;
		this->m_filtering_min = TextureFiltering::Nearest;
		this->m_filtering_mag = TextureFiltering::Nearest;
	}
	else if (preset == Preset::Depth)
	{
		this->m_type = TextureType::Float;
		this->m_format = TextureFormat::Depth;
		this->m_filtering_min = TextureFiltering::Nearest;
		this->m_filtering_mag = TextureFiltering::Nearest;
	}
	else
	{
		throw std::invalid_argument("Unknown preset " + std::to_string(static_cast<int>(preset)));
	}

	this->ConfigureTexture(true);
}

Texture::Texture(const Texture& copy_from)
{
	*this = copy_from;
}

Texture& Texture::operator=(const Texture& copy_from)
{
	this->CopyFrom(copy_from);

	return *this;
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
	this->m_filtering_min = move_from.m_filtering_min;
	this->m_filtering_mag = move_from.m_filtering_mag;
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
	bool reconfigure = false;
	if (filtering != this->m_filtering_min)
	{
		this->m_filtering_min = filtering;
		reconfigure = true;
	}

	if (filtering != this->m_filtering_mag)
	{
		this->m_filtering_mag = filtering;
		reconfigure = true;
	}

	if (reconfigure)
	{
		this->ConfigureTexture(false);
	}
}

void Texture::SetMinFiltering(TextureFiltering min_filtering)
{
	if (min_filtering != this->m_filtering_min)
	{
		this->m_filtering_min = min_filtering;
		this->ConfigureTexture(false);
	}
}

TextureFiltering Texture::GetMinFiltering() const
{
	return this->m_filtering_min;
}

void Texture::SetMagFiltering(TextureFiltering mag_filtering)
{
	if (mag_filtering != this->m_filtering_mag)
	{
		this->m_filtering_mag = mag_filtering;
		this->ConfigureTexture(false);
	}
}

TextureFiltering Texture::GetMagFiltering() const
{
	return this->m_filtering_mag;
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

void Texture::CopyTo(Texture& dest) const
{
	dest.m_dimensions = this->m_dimensions;
	dest.m_type = this->m_type;
	dest.m_format = this->m_format;
	dest.m_filtering_min = this->m_filtering_min;
	dest.m_filtering_mag = this->m_filtering_mag;
	dest.m_target = this->m_target;
	dest.m_generate_mipmaps = this->m_generate_mipmaps;

	dest.m_preferred_format = this->m_preferred_format;

	dest.ConfigureTexture(dest.m_texture == GL_NONE);

	const auto& [width, height] = this->GetDimensions();

	int copy_layers = 0;
	switch (this->GetTargetType())
	{
	case TargetType::Texture_2D: copy_layers = 1; break;
	case TargetType::Texture_Cubemap: copy_layers = 6; break;
	default: throw std::runtime_error("Unknown TargetType " + std::to_string(static_cast<int>(this->GetTargetType())));
	}

	glCopyImageSubData(this->GetTexture(), GetTargetEnum(this->GetTargetType()), 0, 0, 0, 0, dest.GetTexture(), GetTargetEnum(dest.GetTargetType()), 0, 0, 0, 0, width, height, copy_layers);
}

void Texture::CopyFrom(const Texture& src)
{
	src.CopyTo(*this);
}
