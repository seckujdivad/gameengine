#include "TextureFormats.h"

#include <stdexcept>

#include "../GLTextureType.h"
#include "../GLTextureFormat.h"

GLint GetTextureFormatEnum(int num_channels, GLTextureType type, int bit_depth)
{
	if (false) {}
	else if (num_channels == 1 && bit_depth == 0 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_RF
		return GL_RF;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RF");
#endif
	}
	else if (num_channels == 1 && bit_depth == 0 && type == GLTextureType::Integer)
	{
#ifdef GL_R_SNORM
		return GL_R_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_R_SNORM");
#endif
	}
	else if (num_channels == 1 && bit_depth == 0 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_RED
		return GL_RED;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RED");
#endif
	}
	else if (num_channels == 1 && bit_depth == 4 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_R4F
		return GL_R4F;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_R4F");
#endif
	}
	else if (num_channels == 1 && bit_depth == 4 && type == GLTextureType::Integer)
	{
#ifdef GL_R4_SNORM
		return GL_R4_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_R4_SNORM");
#endif
	}
	else if (num_channels == 1 && bit_depth == 4 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_R4
		return GL_R4;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_R4");
#endif
	}
	else if (num_channels == 1 && bit_depth == 8 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_R8F
		return GL_R8F;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_R8F");
#endif
	}
	else if (num_channels == 1 && bit_depth == 8 && type == GLTextureType::Integer)
	{
#ifdef GL_R8_SNORM
		return GL_R8_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_R8_SNORM");
#endif
	}
	else if (num_channels == 1 && bit_depth == 8 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_R8
		return GL_R8;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_R8");
#endif
	}
	else if (num_channels == 1 && bit_depth == 16 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_R16F
		return GL_R16F;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_R16F");
#endif
	}
	else if (num_channels == 1 && bit_depth == 16 && type == GLTextureType::Integer)
	{
#ifdef GL_R16_SNORM
		return GL_R16_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_R16_SNORM");
#endif
	}
	else if (num_channels == 1 && bit_depth == 16 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_R16
		return GL_R16;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_R16");
#endif
	}
	else if (num_channels == 1 && bit_depth == 32 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_R32F
		return GL_R32F;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_R32F");
#endif
	}
	else if (num_channels == 1 && bit_depth == 32 && type == GLTextureType::Integer)
	{
#ifdef GL_R32_SNORM
		return GL_R32_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_R32_SNORM");
#endif
	}
	else if (num_channels == 1 && bit_depth == 32 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_R32
		return GL_R32;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_R32");
#endif
	}
	else if (num_channels == 2 && bit_depth == 0 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_RGF
		return GL_RGF;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGF");
#endif
	}
	else if (num_channels == 2 && bit_depth == 0 && type == GLTextureType::Integer)
	{
#ifdef GL_RG_SNORM
		return GL_RG_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RG_SNORM");
#endif
	}
	else if (num_channels == 2 && bit_depth == 0 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_RG
		return GL_RG;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RG");
#endif
	}
	else if (num_channels == 2 && bit_depth == 4 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_RG4F
		return GL_RG4F;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RG4F");
#endif
	}
	else if (num_channels == 2 && bit_depth == 4 && type == GLTextureType::Integer)
	{
#ifdef GL_RG4_SNORM
		return GL_RG4_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RG4_SNORM");
#endif
	}
	else if (num_channels == 2 && bit_depth == 4 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_RG4
		return GL_RG4;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RG4");
#endif
	}
	else if (num_channels == 2 && bit_depth == 8 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_RG8F
		return GL_RG8F;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RG8F");
#endif
	}
	else if (num_channels == 2 && bit_depth == 8 && type == GLTextureType::Integer)
	{
#ifdef GL_RG8_SNORM
		return GL_RG8_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RG8_SNORM");
#endif
	}
	else if (num_channels == 2 && bit_depth == 8 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_RG8
		return GL_RG8;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RG8");
#endif
	}
	else if (num_channels == 2 && bit_depth == 16 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_RG16F
		return GL_RG16F;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RG16F");
#endif
	}
	else if (num_channels == 2 && bit_depth == 16 && type == GLTextureType::Integer)
	{
#ifdef GL_RG16_SNORM
		return GL_RG16_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RG16_SNORM");
#endif
	}
	else if (num_channels == 2 && bit_depth == 16 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_RG16
		return GL_RG16;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RG16");
#endif
	}
	else if (num_channels == 2 && bit_depth == 32 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_RG32F
		return GL_RG32F;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RG32F");
#endif
	}
	else if (num_channels == 2 && bit_depth == 32 && type == GLTextureType::Integer)
	{
#ifdef GL_RG32_SNORM
		return GL_RG32_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RG32_SNORM");
#endif
	}
	else if (num_channels == 2 && bit_depth == 32 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_RG32
		return GL_RG32;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RG32");
#endif
	}
	else if (num_channels == 3 && bit_depth == 0 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_RGBF
		return GL_RGBF;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGBF");
#endif
	}
	else if (num_channels == 3 && bit_depth == 0 && type == GLTextureType::Integer)
	{
#ifdef GL_RGB_SNORM
		return GL_RGB_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGB_SNORM");
#endif
	}
	else if (num_channels == 3 && bit_depth == 0 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_RGB
		return GL_RGB;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGB");
#endif
	}
	else if (num_channels == 3 && bit_depth == 4 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_RGB4F
		return GL_RGB4F;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGB4F");
#endif
	}
	else if (num_channels == 3 && bit_depth == 4 && type == GLTextureType::Integer)
	{
#ifdef GL_RGB4_SNORM
		return GL_RGB4_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGB4_SNORM");
#endif
	}
	else if (num_channels == 3 && bit_depth == 4 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_RGB4
		return GL_RGB4;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGB4");
#endif
	}
	else if (num_channels == 3 && bit_depth == 8 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_RGB8F
		return GL_RGB8F;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGB8F");
#endif
	}
	else if (num_channels == 3 && bit_depth == 8 && type == GLTextureType::Integer)
	{
#ifdef GL_RGB8_SNORM
		return GL_RGB8_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGB8_SNORM");
#endif
	}
	else if (num_channels == 3 && bit_depth == 8 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_RGB8
		return GL_RGB8;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGB8");
#endif
	}
	else if (num_channels == 3 && bit_depth == 16 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_RGB16F
		return GL_RGB16F;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGB16F");
#endif
	}
	else if (num_channels == 3 && bit_depth == 16 && type == GLTextureType::Integer)
	{
#ifdef GL_RGB16_SNORM
		return GL_RGB16_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGB16_SNORM");
#endif
	}
	else if (num_channels == 3 && bit_depth == 16 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_RGB16
		return GL_RGB16;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGB16");
#endif
	}
	else if (num_channels == 3 && bit_depth == 32 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_RGB32F
		return GL_RGB32F;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGB32F");
#endif
	}
	else if (num_channels == 3 && bit_depth == 32 && type == GLTextureType::Integer)
	{
#ifdef GL_RGB32_SNORM
		return GL_RGB32_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGB32_SNORM");
#endif
	}
	else if (num_channels == 3 && bit_depth == 32 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_RGB32
		return GL_RGB32;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGB32");
#endif
	}
	else if (num_channels == 4 && bit_depth == 0 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_RGBAF
		return GL_RGBAF;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGBAF");
#endif
	}
	else if (num_channels == 4 && bit_depth == 0 && type == GLTextureType::Integer)
	{
#ifdef GL_RGBA_SNORM
		return GL_RGBA_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGBA_SNORM");
#endif
	}
	else if (num_channels == 4 && bit_depth == 0 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_RGBA
		return GL_RGBA;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGBA");
#endif
	}
	else if (num_channels == 4 && bit_depth == 4 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_RGBA4F
		return GL_RGBA4F;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGBA4F");
#endif
	}
	else if (num_channels == 4 && bit_depth == 4 && type == GLTextureType::Integer)
	{
#ifdef GL_RGBA4_SNORM
		return GL_RGBA4_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGBA4_SNORM");
#endif
	}
	else if (num_channels == 4 && bit_depth == 4 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_RGBA4
		return GL_RGBA4;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGBA4");
#endif
	}
	else if (num_channels == 4 && bit_depth == 8 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_RGBA8F
		return GL_RGBA8F;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGBA8F");
#endif
	}
	else if (num_channels == 4 && bit_depth == 8 && type == GLTextureType::Integer)
	{
#ifdef GL_RGBA8_SNORM
		return GL_RGBA8_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGBA8_SNORM");
#endif
	}
	else if (num_channels == 4 && bit_depth == 8 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_RGBA8
		return GL_RGBA8;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGBA8");
#endif
	}
	else if (num_channels == 4 && bit_depth == 16 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_RGBA16F
		return GL_RGBA16F;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGBA16F");
#endif
	}
	else if (num_channels == 4 && bit_depth == 16 && type == GLTextureType::Integer)
	{
#ifdef GL_RGBA16_SNORM
		return GL_RGBA16_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGBA16_SNORM");
#endif
	}
	else if (num_channels == 4 && bit_depth == 16 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_RGBA16
		return GL_RGBA16;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGBA16");
#endif
	}
	else if (num_channels == 4 && bit_depth == 32 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat))
	{
#ifdef GL_RGBA32F
		return GL_RGBA32F;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGBA32F");
#endif
	}
	else if (num_channels == 4 && bit_depth == 32 && type == GLTextureType::Integer)
	{
#ifdef GL_RGBA32_SNORM
		return GL_RGBA32_SNORM;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGBA32_SNORM");
#endif
	}
	else if (num_channels == 4 && bit_depth == 32 && type == GLTextureType::UnsignedByte)
	{
#ifdef GL_RGBA32
		return GL_RGBA32;
#else
		throw std::invalid_argument("The GLEW headers used when compiling didn't contain GL_RGBA32");
#endif
	}
	else
	{
		throw std::invalid_argument("At least one argument was invalid and not caught by the wrapper function");
	}
}