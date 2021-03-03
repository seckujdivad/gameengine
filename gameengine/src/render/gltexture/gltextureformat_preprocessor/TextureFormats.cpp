#include "TextureFormats.h"

#include <stdexcept>

#include "../GLTextureType.h"
#include "../GLTextureFormat.h"

GLint GetTextureFormatEnum(int num_channels, GLTextureType type, int bit_depth)
{
	if (false) {}
#ifdef GL_RF
	else if (num_channels == 1 && bit_depth == 0 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_RF;}
#endif
#ifdef GL_R_SNORM
	else if (num_channels == 1 && bit_depth == 0 && type == GLTextureType::Integer) {return GL_R_SNORM;}
#endif
#ifdef GL_RED
	else if (num_channels == 1 && bit_depth == 0 && type == GLTextureType::UnsignedByte) {return GL_RED;}
#endif
#ifdef GL_R4F
	else if (num_channels == 1 && bit_depth == 4 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_R4F;}
#endif
#ifdef GL_R4_SNORM
	else if (num_channels == 1 && bit_depth == 4 && type == GLTextureType::Integer) {return GL_R4_SNORM;}
#endif
#ifdef GL_R4
	else if (num_channels == 1 && bit_depth == 4 && type == GLTextureType::UnsignedByte) {return GL_R4;}
#endif
#ifdef GL_R8F
	else if (num_channels == 1 && bit_depth == 8 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_R8F;}
#endif
#ifdef GL_R8_SNORM
	else if (num_channels == 1 && bit_depth == 8 && type == GLTextureType::Integer) {return GL_R8_SNORM;}
#endif
#ifdef GL_R8
	else if (num_channels == 1 && bit_depth == 8 && type == GLTextureType::UnsignedByte) {return GL_R8;}
#endif
#ifdef GL_R16F
	else if (num_channels == 1 && bit_depth == 16 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_R16F;}
#endif
#ifdef GL_R16_SNORM
	else if (num_channels == 1 && bit_depth == 16 && type == GLTextureType::Integer) {return GL_R16_SNORM;}
#endif
#ifdef GL_R16
	else if (num_channels == 1 && bit_depth == 16 && type == GLTextureType::UnsignedByte) {return GL_R16;}
#endif
#ifdef GL_R32F
	else if (num_channels == 1 && bit_depth == 32 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_R32F;}
#endif
#ifdef GL_R32_SNORM
	else if (num_channels == 1 && bit_depth == 32 && type == GLTextureType::Integer) {return GL_R32_SNORM;}
#endif
#ifdef GL_R32
	else if (num_channels == 1 && bit_depth == 32 && type == GLTextureType::UnsignedByte) {return GL_R32;}
#endif
#ifdef GL_RGF
	else if (num_channels == 2 && bit_depth == 0 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_RGF;}
#endif
#ifdef GL_RG_SNORM
	else if (num_channels == 2 && bit_depth == 0 && type == GLTextureType::Integer) {return GL_RG_SNORM;}
#endif
#ifdef GL_RG
	else if (num_channels == 2 && bit_depth == 0 && type == GLTextureType::UnsignedByte) {return GL_RG;}
#endif
#ifdef GL_RG4F
	else if (num_channels == 2 && bit_depth == 4 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_RG4F;}
#endif
#ifdef GL_RG4_SNORM
	else if (num_channels == 2 && bit_depth == 4 && type == GLTextureType::Integer) {return GL_RG4_SNORM;}
#endif
#ifdef GL_RG4
	else if (num_channels == 2 && bit_depth == 4 && type == GLTextureType::UnsignedByte) {return GL_RG4;}
#endif
#ifdef GL_RG8F
	else if (num_channels == 2 && bit_depth == 8 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_RG8F;}
#endif
#ifdef GL_RG8_SNORM
	else if (num_channels == 2 && bit_depth == 8 && type == GLTextureType::Integer) {return GL_RG8_SNORM;}
#endif
#ifdef GL_RG8
	else if (num_channels == 2 && bit_depth == 8 && type == GLTextureType::UnsignedByte) {return GL_RG8;}
#endif
#ifdef GL_RG16F
	else if (num_channels == 2 && bit_depth == 16 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_RG16F;}
#endif
#ifdef GL_RG16_SNORM
	else if (num_channels == 2 && bit_depth == 16 && type == GLTextureType::Integer) {return GL_RG16_SNORM;}
#endif
#ifdef GL_RG16
	else if (num_channels == 2 && bit_depth == 16 && type == GLTextureType::UnsignedByte) {return GL_RG16;}
#endif
#ifdef GL_RG32F
	else if (num_channels == 2 && bit_depth == 32 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_RG32F;}
#endif
#ifdef GL_RG32_SNORM
	else if (num_channels == 2 && bit_depth == 32 && type == GLTextureType::Integer) {return GL_RG32_SNORM;}
#endif
#ifdef GL_RG32
	else if (num_channels == 2 && bit_depth == 32 && type == GLTextureType::UnsignedByte) {return GL_RG32;}
#endif
#ifdef GL_RGBF
	else if (num_channels == 3 && bit_depth == 0 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_RGBF;}
#endif
#ifdef GL_RGB_SNORM
	else if (num_channels == 3 && bit_depth == 0 && type == GLTextureType::Integer) {return GL_RGB_SNORM;}
#endif
#ifdef GL_RGB
	else if (num_channels == 3 && bit_depth == 0 && type == GLTextureType::UnsignedByte) {return GL_RGB;}
#endif
#ifdef GL_RGB4F
	else if (num_channels == 3 && bit_depth == 4 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_RGB4F;}
#endif
#ifdef GL_RGB4_SNORM
	else if (num_channels == 3 && bit_depth == 4 && type == GLTextureType::Integer) {return GL_RGB4_SNORM;}
#endif
#ifdef GL_RGB4
	else if (num_channels == 3 && bit_depth == 4 && type == GLTextureType::UnsignedByte) {return GL_RGB4;}
#endif
#ifdef GL_RGB8F
	else if (num_channels == 3 && bit_depth == 8 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_RGB8F;}
#endif
#ifdef GL_RGB8_SNORM
	else if (num_channels == 3 && bit_depth == 8 && type == GLTextureType::Integer) {return GL_RGB8_SNORM;}
#endif
#ifdef GL_RGB8
	else if (num_channels == 3 && bit_depth == 8 && type == GLTextureType::UnsignedByte) {return GL_RGB8;}
#endif
#ifdef GL_RGB16F
	else if (num_channels == 3 && bit_depth == 16 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_RGB16F;}
#endif
#ifdef GL_RGB16_SNORM
	else if (num_channels == 3 && bit_depth == 16 && type == GLTextureType::Integer) {return GL_RGB16_SNORM;}
#endif
#ifdef GL_RGB16
	else if (num_channels == 3 && bit_depth == 16 && type == GLTextureType::UnsignedByte) {return GL_RGB16;}
#endif
#ifdef GL_RGB32F
	else if (num_channels == 3 && bit_depth == 32 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_RGB32F;}
#endif
#ifdef GL_RGB32_SNORM
	else if (num_channels == 3 && bit_depth == 32 && type == GLTextureType::Integer) {return GL_RGB32_SNORM;}
#endif
#ifdef GL_RGB32
	else if (num_channels == 3 && bit_depth == 32 && type == GLTextureType::UnsignedByte) {return GL_RGB32;}
#endif
#ifdef GL_RGBAF
	else if (num_channels == 4 && bit_depth == 0 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_RGBAF;}
#endif
#ifdef GL_RGBA_SNORM
	else if (num_channels == 4 && bit_depth == 0 && type == GLTextureType::Integer) {return GL_RGBA_SNORM;}
#endif
#ifdef GL_RGBA
	else if (num_channels == 4 && bit_depth == 0 && type == GLTextureType::UnsignedByte) {return GL_RGBA;}
#endif
#ifdef GL_RGBA4F
	else if (num_channels == 4 && bit_depth == 4 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_RGBA4F;}
#endif
#ifdef GL_RGBA4_SNORM
	else if (num_channels == 4 && bit_depth == 4 && type == GLTextureType::Integer) {return GL_RGBA4_SNORM;}
#endif
#ifdef GL_RGBA4
	else if (num_channels == 4 && bit_depth == 4 && type == GLTextureType::UnsignedByte) {return GL_RGBA4;}
#endif
#ifdef GL_RGBA8F
	else if (num_channels == 4 && bit_depth == 8 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_RGBA8F;}
#endif
#ifdef GL_RGBA8_SNORM
	else if (num_channels == 4 && bit_depth == 8 && type == GLTextureType::Integer) {return GL_RGBA8_SNORM;}
#endif
#ifdef GL_RGBA8
	else if (num_channels == 4 && bit_depth == 8 && type == GLTextureType::UnsignedByte) {return GL_RGBA8;}
#endif
#ifdef GL_RGBA16F
	else if (num_channels == 4 && bit_depth == 16 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_RGBA16F;}
#endif
#ifdef GL_RGBA16_SNORM
	else if (num_channels == 4 && bit_depth == 16 && type == GLTextureType::Integer) {return GL_RGBA16_SNORM;}
#endif
#ifdef GL_RGBA16
	else if (num_channels == 4 && bit_depth == 16 && type == GLTextureType::UnsignedByte) {return GL_RGBA16;}
#endif
#ifdef GL_RGBA32F
	else if (num_channels == 4 && bit_depth == 32 && (type == GLTextureType::Float || type == GLTextureType::HalfFloat)) {return GL_RGBA32F;}
#endif
#ifdef GL_RGBA32_SNORM
	else if (num_channels == 4 && bit_depth == 32 && type == GLTextureType::Integer) {return GL_RGBA32_SNORM;}
#endif
#ifdef GL_RGBA32
	else if (num_channels == 4 && bit_depth == 32 && type == GLTextureType::UnsignedByte) {return GL_RGBA32;}
#endif
	else
	{
		throw std::invalid_argument("Unknown format");
	}
}