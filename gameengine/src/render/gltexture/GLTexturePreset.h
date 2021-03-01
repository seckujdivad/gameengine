#pragma once

#include <cstddef>

enum class TargetType;
enum class GLTextureDataPreset;

struct GLTexturePreset
{
	GLTexturePreset();
	GLTexturePreset(TargetType target, GLTextureDataPreset preset);

	TargetType target;
	GLTextureDataPreset preset;

	bool operator==(const GLTexturePreset& other) const;
	bool operator!=(const GLTexturePreset& other) const;

	struct Hash
	{
		std::size_t operator()(const GLTexturePreset& to_hash) const;
	};
};