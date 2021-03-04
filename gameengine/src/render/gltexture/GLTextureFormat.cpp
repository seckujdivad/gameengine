#include "GLTextureFormat.h"

#include <stdexcept>
#include <string>

#include "GLTextureType.h"

#include "gltextureformat_preprocessor/TextureFormats.h"

GLint GetTextureFormatEnum(GLTextureFormat format)
{
	if (format.index() == 0)
	{
		GLTextureFormat_Colour& format_data = std::get<GLTextureFormat_Colour>(format);

		if (format_data.num_channels < 1 || format_data.num_channels > 4)
		{
			throw std::invalid_argument("\"num_channels\" must be in range [1, 4]");
		}

		if (format_data.bit_depth != 0
			&& format_data.bit_depth != 4
			&& format_data.bit_depth != 8
			&& format_data.bit_depth != 16
			&& format_data.bit_depth != 32)
		{
			throw std::invalid_argument("\"bit_depth\" must be in set {0, 4, 8, 16, 32}");
		}

		if (format_data.type != GLTextureType::Float
			&& format_data.type != GLTextureType::HalfFloat
			&& format_data.type != GLTextureType::UnsignedByte
			&& format_data.type != GLTextureType::Integer)
		{
			throw std::invalid_argument("\"type\" must be in set {Float, HalfFloat, UnsignedByte, Integer}");
		}

		return GetTextureFormatEnum(format_data.num_channels, format_data.type, format_data.bit_depth);
	}
	else
	{
		return GL_DEPTH_COMPONENT;
	}
}

GLTextureFormat_Colour::GLTextureFormat_Colour(int num_channels, GLTextureType type, int bit_depth) : num_channels(num_channels), type(type), bit_depth(bit_depth)
{
}

bool GLTextureFormat_Colour::operator==(const GLTextureFormat_Colour& compare) const
{
	if (this->num_channels != compare.num_channels)
	{
		return false;
	}

	if (this->type != compare.type)
	{
		return false;
	}

	if (this->bit_depth != compare.bit_depth)
	{
		return false;
	}

	return true;
}

bool GLTextureFormat_Colour::operator!=(const GLTextureFormat_Colour& compare) const
{
	return !(*this == compare);
}

bool GLTextureFormat_Depth::operator==(const GLTextureFormat_Depth& compare) const
{
	return true;
}

bool GLTextureFormat_Depth::operator!=(const GLTextureFormat_Depth& compare) const
{
	return !(*this == compare);
}
