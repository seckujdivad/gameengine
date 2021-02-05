#pragma once

#include <tuple>
#include <vector>
#include <optional>

#include "../../GLComponents.h"

enum class TextureType;
enum class TextureFormat;
enum class TextureFiltering;
enum class TargetType;

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
	TextureFiltering m_filtering;
	TargetType m_target;
	bool m_generate_mipmaps;

	GLuint m_texture;
	GLint m_preferred_format = GL_NONE;

private:
	void ConfigureTexture(bool create, std::optional<TextureFormat> pixel_format = std::optional<TextureFormat>(), std::vector<const void*> pixels = {});
	GLint GetPreferredFormat(bool force = false);

public:
	Texture(Preset preset, TargetType target, std::tuple<int, int> dimensions, bool generate_mipmaps);
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
	TextureFiltering GetFiltering() const;

	void SetTargetType(TargetType target);
	TargetType GetTargetType() const;

	void SetGenerateMipMaps(bool generate_mipmaps);
	bool GetGenerateMipMaps() const;

	GLuint GetTexture() const;
	void BindTexture() const;

	void SetPixels(TextureFormat pixel_format, std::vector<const void*> pixels);

	void CopyTo(Texture& dest) const;
	void CopyFrom(const Texture& src);
};