#include "GLTexturePreset.h"

#include "../TargetType.h"
#include "GLTextureDataPreset.h"

GLTexturePreset::GLTexturePreset()
{
	this->target = TargetType::Texture_2D;
	this->preset = GLTextureDataPreset::Black;
}

GLTexturePreset::GLTexturePreset(TargetType target, GLTextureDataPreset preset) : target(target), preset(preset)
{
}

bool GLTexturePreset::operator==(const GLTexturePreset& other) const
{
	if (this->preset != other.preset)
	{
		return false;
	}

	if (this->target != other.target)
	{
		return false;
	}

	return true;
}

bool GLTexturePreset::operator!=(const GLTexturePreset& other) const
{
	return !(*this == other);
}

std::size_t GLTexturePreset::Hash::operator()(const GLTexturePreset& to_hash) const
{
	return std::size_t(to_hash.target) + (std::size_t(to_hash.preset) << 8);
}
