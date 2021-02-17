#include "TexturePreset.h"

#include "../TargetType.h"
#include "TextureDataPreset.h"

TexturePreset::TexturePreset()
{
}

TexturePreset::TexturePreset(TargetType target, TextureDataPreset preset) : target(target), preset(preset)
{
}

bool TexturePreset::operator==(const TexturePreset& other) const
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

bool TexturePreset::operator!=(const TexturePreset& other) const
{
	return !(*this == other);
}

std::size_t TexturePreset::Hash::operator()(const TexturePreset& to_hash) const
{
	return std::size_t(to_hash.target) + (std::size_t(to_hash.preset) << 8);
}
