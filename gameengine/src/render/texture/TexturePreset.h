#pragma once

#include <cstddef>

enum class TargetType;
enum class TextureDataPreset;

struct TexturePreset
{
	TexturePreset();
	TexturePreset(TargetType target, TextureDataPreset preset);

	TargetType target;
	TextureDataPreset preset;

	bool operator==(const TexturePreset& other) const;
	bool operator!=(const TexturePreset& other) const;

	struct Hash
	{
		std::size_t operator()(const TexturePreset& to_hash) const;
	};
};