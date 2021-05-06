#include "GLTexture.h"

#include <stdexcept>
#include <string>

#include "GLTextureType.h"
#include "GLTextureFormat.h"
#include "GLTextureDataPreset.h"
#include "../TargetType.h"
#include "../rendertarget/target/RenderTargetMode.h"
#include "../../scene/texture/TextureFiltering.h"

void GLTexture::ConfigureTexture(bool create, std::optional<GLTextureFormat> pixel_format, std::vector<const void*> pixels)
{
	//pad pixel data with nullptr
	std::size_t req_num_pixel_arrays = 0;
	switch (this->GetTargetType())
	{
	case (TargetType::Texture_2D): req_num_pixel_arrays = 1; break;
	case (TargetType::Texture_Cubemap): req_num_pixel_arrays = 6; break;
	default: throw std::invalid_argument("Unrecognised target type");
	}

	for (std::size_t i = pixels.size(); i < req_num_pixel_arrays; i++)
	{
		pixels.push_back(nullptr);
	}

	if (pixels.size() > req_num_pixel_arrays)
	{
		throw std::invalid_argument("Too many pixel arrays provided - " + std::to_string(req_num_pixel_arrays) + " required");
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
		this->SetName(this->m_texture);
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
		const GLint wrapping_mode = GL_CLAMP_TO_EDGE;
		glTexParameteri(target, GL_TEXTURE_WRAP_R, wrapping_mode);
		glTexParameteri(target, GL_TEXTURE_WRAP_S, wrapping_mode);
		glTexParameteri(target, GL_TEXTURE_WRAP_T, wrapping_mode);
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

GLint GLTexture::GetPreferredFormat(bool force)
{
	if (force || (this->m_preferred_format == GL_NONE))
	{
		glGetInternalformativ(GetTargetEnum(this->GetTargetType()), GetTextureFormatEnum(this->GetFormat()), GL_TEXTURE_IMAGE_FORMAT, 1, &this->m_preferred_format);

		if (this->m_preferred_format == GL_NONE)
		{
			this->m_preferred_format = GetSimpleTextureFormatEnum(this->GetFormat());
		}
	}
	return this->m_preferred_format;
}

void GLTexture::SetPreset(Preset preset, std::optional<int> num_channels, bool configure)
{
	int actual_num_channels;
	if (num_channels.has_value())
	{
		actual_num_channels = num_channels.value();
	}
	else
	{
		actual_num_channels = 4;
	}

	if (preset == Preset::Colour)
	{
		this->m_type = GLTextureType::UnsignedByte;
		this->m_format = GLTextureFormat_Colour(actual_num_channels, this->m_type);
		this->m_filtering_min = TextureFiltering::Linear;
		this->m_filtering_mag = TextureFiltering::Linear;
	}
	else if (preset == Preset::Data_MediumP)
	{
		this->m_type = GLTextureType::HalfFloat;
		this->m_format = GLTextureFormat_Colour(actual_num_channels, this->m_type, 16);
		this->m_filtering_min = TextureFiltering::Nearest;
		this->m_filtering_mag = TextureFiltering::Nearest;
	}
	else if (preset == Preset::Data_LowP)
	{
		this->m_type = GLTextureType::UnsignedByte;
		this->m_format = GLTextureFormat_Colour(actual_num_channels, this->m_type, 8);
		this->m_filtering_min = TextureFiltering::Nearest;
		this->m_filtering_mag = TextureFiltering::Nearest;
	}
	else if (preset == Preset::Depth)
	{
		this->m_type = GLTextureType::Float;
		this->m_format = GLTextureFormat_Depth();
		this->m_filtering_min = TextureFiltering::Nearest;
		this->m_filtering_mag = TextureFiltering::Nearest;
	}
	else
	{
		throw std::invalid_argument("Unknown preset " + std::to_string(static_cast<int>(preset)));
	}
}

GLTexture::GLTexture(Preset preset, TargetType target, std::optional<int> num_channels, std::tuple<int, int> dimensions, bool generate_mipmaps) : m_dimensions(dimensions), m_target(target), m_generate_mipmaps(generate_mipmaps), m_type(GLTextureType::UnsignedByte), m_format(GLTextureFormat_Colour(4, m_type)), GLObjectLabelable(GL_TEXTURE)
{
	this->SetPreset(preset, num_channels, false);
	this->ConfigureTexture(true);
}

GLTexture::GLTexture(GLTextureDataPreset preset, TargetType target, std::optional<int> num_channels, bool generate_mipmaps) : m_dimensions(std::tuple(1, 1)), m_target(target), m_generate_mipmaps(generate_mipmaps), m_type(GLTextureType::UnsignedByte), m_format(GLTextureFormat_Colour(4, m_type)), GLObjectLabelable(GL_TEXTURE)
{
	this->SetPixels(preset);
}

GLTexture::GLTexture(GLTexturePreset preset, bool generate_mipmaps) : GLTexture(preset.preset, preset.target, generate_mipmaps)
{
}

GLTexture::GLTexture(const GLTexture& copy_from) : m_format(copy_from.m_format), GLObjectLabelable(GL_TEXTURE)
{
	*this = copy_from;
}

GLTexture& GLTexture::operator=(const GLTexture& copy_from)
{
	this->CopyFrom(copy_from);

	return *this;
}

GLTexture::GLTexture(GLTexture&& move_from) noexcept : m_format(std::move(move_from.m_format)), GLObjectLabelable(GL_TEXTURE)
{
	*this = std::move(move_from);
}

GLTexture& GLTexture::operator=(GLTexture&& move_from) noexcept
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

GLTexture::~GLTexture()
{
	if (this->m_texture != GL_NONE)
	{
		glDeleteTextures(1, &this->m_texture);
	}
}

bool GLTexture::SetDimensions(std::tuple<int, int> dimensions)
{
	if (dimensions == this->m_dimensions)
	{
		return false;
	}
	else
	{
		this->m_dimensions = dimensions;
		this->ConfigureTexture(false);
		return true;
	}
}

std::tuple<int, int> GLTexture::GetDimensions() const
{
	return this->m_dimensions;
}

void GLTexture::SetTextureType(GLTextureType type)
{
	if (type != this->m_type)
	{
		this->m_type = type;
		this->ConfigureTexture(false);
	}
}

GLTextureType GLTexture::GetTextureType() const
{
	return this->m_type;
}

void GLTexture::SetFormat(GLTextureFormat format)
{
	if (format != this->m_format)
	{
		this->m_format = format;
		this->GetPreferredFormat(true);
		this->ConfigureTexture(false);
	}
}

GLTextureFormat GLTexture::GetFormat() const
{
	return this->m_format;
}

void GLTexture::SetFiltering(TextureFiltering filtering)
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

void GLTexture::SetMinFiltering(TextureFiltering min_filtering)
{
	if (min_filtering != this->m_filtering_min)
	{
		this->m_filtering_min = min_filtering;
		this->ConfigureTexture(false);
	}
}

TextureFiltering GLTexture::GetMinFiltering() const
{
	return this->m_filtering_min;
}

void GLTexture::SetMagFiltering(TextureFiltering mag_filtering)
{
	if (mag_filtering != this->m_filtering_mag)
	{
		this->m_filtering_mag = mag_filtering;
		this->ConfigureTexture(false);
	}
}

TextureFiltering GLTexture::GetMagFiltering() const
{
	return this->m_filtering_mag;
}

TargetType GLTexture::GetTargetType() const
{
	return this->m_target;
}

void GLTexture::SetGenerateMipMaps(bool generate_mipmaps)
{
	if (generate_mipmaps != this->m_generate_mipmaps)
	{
		this->m_generate_mipmaps = generate_mipmaps;
		this->ConfigureTexture(false);
	}
}

bool GLTexture::GetGenerateMipMaps() const
{
	return false;
}

GLuint GLTexture::GetTexture() const
{
	return this->m_texture;
}

void GLTexture::BindTexture() const
{
	glBindTexture(GetTargetEnum(this->GetTargetType()), this->GetTexture());
}

void GLTexture::SetPixels(GLTextureFormat pixel_format, std::vector<const void*> pixels)
{
	this->ConfigureTexture(false, pixel_format, pixels);
}

void GLTexture::SetPixels(GLTextureDataPreset preset)
{
	if (preset == GLTextureDataPreset::Black)
	{
		this->SetDimensions(std::tuple(1, 1));

		this->SetPreset(Preset::Colour, 3);
		std::vector<unsigned char> black_texture; //make sure it stays allocated
		black_texture.reserve(3);
		for (int i = 0; i < 3; i++)
		{
			black_texture.push_back(0);
		}

		std::vector<const void*> pixel_arrays;
		for (int i = 0; i < GetNumTextures(this->GetTargetType()); i++)
		{
			pixel_arrays.push_back(black_texture.data());
		}

		this->SetPixels(GLTextureFormat_Colour(3, GLTextureType::UnsignedByte), pixel_arrays);
	}
	else if (preset == GLTextureDataPreset::ZeroDepth)
	{
		this->SetDimensions(std::tuple(1, 1));
		this->SetPreset(Preset::Depth);

		std::vector<float> zerodepth_texture;
		zerodepth_texture.reserve(1);
		zerodepth_texture.push_back(0.0f);

		std::vector<const void*> pixel_arrays;
		for (int i = 0; i < GetNumTextures(this->GetTargetType()); i++)
		{
			pixel_arrays.push_back(zerodepth_texture.data());
		}

		this->SetPixels(GLTextureFormat_Depth(), pixel_arrays);
	}
	else if (preset == GLTextureDataPreset::ZeroShadow)
	{
		this->SetPixels(GLTextureDataPreset::ZeroDepth);
		this->SetTexParameter(GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
		this->SetTexParameter(GL_TEXTURE_COMPARE_FUNC, GL_LEQUAL);
	}
	else
	{
		throw std::invalid_argument("Unknown texture preset " + std::to_string(static_cast<int>(preset)));
	}
}

void GLTexture::CopyTo(GLTexture& dest) const
{
	if ((&dest == this) || (this->GetTexture() == dest.GetTexture()))
	{
		throw std::invalid_argument("Can't copy a texture onto itself");
	}

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

	dest.SetName(dest.m_texture);

	dest.SetTexParameter(GL_TEXTURE_COMPARE_MODE, this->GetTexParameteri(GL_TEXTURE_COMPARE_MODE));
	dest.SetTexParameter(GL_TEXTURE_COMPARE_FUNC, this->GetTexParameteri(GL_TEXTURE_COMPARE_FUNC));
}

void GLTexture::CopyFrom(const GLTexture& src)
{
	src.CopyTo(*this);
}

void GLTexture::SetTexParameter(GLenum pname, GLint param)
{
	this->BindTexture();
	glTexParameteri(GetTargetEnum(this->GetTargetType()), pname, param);
}

void GLTexture::SetTexParameter(GLenum pname, std::vector<GLint> params)
{
	this->BindTexture();
	glTexParameteriv(GetTargetEnum(this->GetTargetType()), pname, params.data());
}

std::vector<GLint> GLTexture::GetTexParameteriv(GLenum pname, std::size_t num_params) const
{
	std::unique_ptr<GLint[]> output = std::make_unique<GLint[]>(num_params);

	this->BindTexture();
	glGetTexParameteriv(GetTargetEnum(this->GetTargetType()), pname, output.get());

	std::vector<GLint> result;
	result.reserve(num_params);
	for (std::size_t i = 0; i < num_params; i++)
	{
		result.push_back(output[i]);
	}

	return result;
}

std::vector<GLfloat> GLTexture::GetTexParameterfv(GLenum pname, std::size_t num_params) const
{
	std::unique_ptr<GLfloat[]> output = std::make_unique<GLfloat[]>(num_params);

	this->BindTexture();
	glGetTexParameterfv(GetTargetEnum(this->GetTargetType()), pname, output.get());

	std::vector<GLfloat> result;
	result.reserve(num_params);
	for (std::size_t i = 0; i < num_params; i++)
	{
		result.push_back(output[i]);
	}

	return result;
}

GLint GLTexture::GetTexParameteri(GLenum pname) const
{
	GLint result = GL_NONE;
	glGetTexParameteriv(GetTargetEnum(this->GetTargetType()), pname, &result);
	return result;
}

GLfloat GLTexture::GetTexParameterf(GLenum pname) const
{
	GLfloat result = 0.0f;
	glGetTexParameterfv(GetTargetEnum(this->GetTargetType()), pname, &result);
	return result;
}

void GLTexture::SetTexParameter(GLenum pname, GLfloat param)
{
	this->BindTexture();
	glTexParameterf(GetTargetEnum(this->GetTargetType()), pname, param);
}

void GLTexture::SetTexParameter(GLenum pname, std::vector<GLfloat> params)
{
	this->BindTexture();
	glTexParameterfv(GetTargetEnum(this->GetTargetType()), pname, params.data());
}

std::optional<int> GetNumColourTextures(RenderTargetMode mode)
{
	if (mode == RenderTargetMode::Default)
	{
		throw std::invalid_argument("Default mode can't be rendered");
	}
	else if (mode == RenderTargetMode::Normal_DepthOnly)
	{
		return 1;
	}
	else if (mode == RenderTargetMode::Normal_Draw)
	{
		return 3;
	}
	else if (mode == RenderTargetMode::Normal_SSRQuality)
	{
		return 1;
	}
	else if (mode == RenderTargetMode::Normal_PostProcess)
	{
		return 1;
	}
	else if (mode == RenderTargetMode::PostProcess)
	{
		return 1;
	}
	else if (mode == RenderTargetMode::Shadow)
	{
		return 0;
	}
	else if (mode == RenderTargetMode::Textured)
	{
		return 1;
	}
	else if (mode == RenderTargetMode::Wireframe)
	{
		return 1;
	}
	else
	{
		throw std::invalid_argument("Unknown mode");
	}
}

int GetNumAttachedColourTextures(RenderTargetMode mode)
{
	return GetNumColourTextures(mode).value();
}
