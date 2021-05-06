#pragma once

#include <cstddef>
#include <string>

enum class TargetType;
enum class GLTextureDataPreset;

struct GLTexturePreset
{
	GLTexturePreset();
	GLTexturePreset(TargetType target, GLTextureDataPreset preset);

	std::string ToString() const;

	TargetType target;
	GLTextureDataPreset preset;

	bool operator==(const GLTexturePreset& other) const;
	bool operator!=(const GLTexturePreset& other) const;

	struct Hash
	{
		std::size_t operator()(const GLTexturePreset& to_hash) const;
	};
};