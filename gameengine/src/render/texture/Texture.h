#pragma once

#include <tuple>
#include <vector>
#include <optional>

#include "../../GLComponents.h"

#include "TexturePreset.h"

enum class TextureType;
enum class TextureFormat;
enum class TextureFiltering;
enum class TargetType;
enum class TextureDataPreset;

class Texture
{
public:
	enum class Preset
	{
		Colour,
		Data,
		Depth
	};

private:
	std::tuple<int, int> m_dimensions;

	TextureType m_type;
	TextureFormat m_format;
	TextureFiltering m_filtering_min;
	TextureFiltering m_filtering_mag;
	TargetType m_target;
	bool m_generate_mipmaps;

	GLuint m_texture = GL_NONE;
	GLint m_preferred_format = GL_NONE;

private:
	void ConfigureTexture(bool create, std::optional<TextureFormat> pixel_format = std::optional<TextureFormat>(), std::vector<const void*> pixels = {});
	GLint GetPreferredFormat(bool force = false);

	void SetPreset(Preset preset, bool configure = true);

public:
	Texture(Preset preset, TargetType target, std::tuple<int, int> dimensions = std::tuple<int, int>(1, 1), bool generate_mipmaps = false);
	Texture(TextureDataPreset preset, TargetType target, bool generate_mipmaps = false);
	Texture(TexturePreset preset, bool generate_mipmaps = false);
	Texture(const Texture& copy_from);
	Texture& operator=(const Texture& copy_from);
	Texture(Texture&& move_from) noexcept;
	Texture& operator=(Texture&& move_from) noexcept;
	~Texture();

	void SetDimensions(std::tuple<int, int> dimensions);
	std::tuple<int, int> GetDimensions() const;

	void SetTextureType(TextureType type);
	TextureType GetTextureType() const;

	void SetFormat(TextureFormat format);
	TextureFormat GetFormat() const;

	void SetFiltering(TextureFiltering filtering);

	void SetMinFiltering(TextureFiltering min_filtering);
	TextureFiltering GetMinFiltering() const;

	void SetMagFiltering(TextureFiltering mag_filtering);
	TextureFiltering GetMagFiltering() const;

	TargetType GetTargetType() const;

	void SetGenerateMipMaps(bool generate_mipmaps);
	bool GetGenerateMipMaps() const;

	GLuint GetTexture() const;
	void BindTexture() const;

	void SetPixels(TextureFormat pixel_format, std::vector<const void*> pixels);
	void SetPixels(TextureDataPreset preset);

	void CopyTo(Texture& dest) const;
	void CopyFrom(const Texture& src);
};