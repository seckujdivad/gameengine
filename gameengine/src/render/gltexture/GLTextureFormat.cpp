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
