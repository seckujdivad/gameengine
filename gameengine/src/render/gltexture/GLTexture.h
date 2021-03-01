#pragma once

#include <tuple>
#include <vector>
#include <optional>

#include "../../GLComponents.h"

#include "GLTexturePreset.h"

enum class GLTextureType;
enum class GLTextureFormat;
enum class GLTextureFiltering;
enum class TargetType;
enum class GLTextureDataPreset;

enum class RenderTargetMode;

class GLTexture
{
public:
	enum class Preset
	{
		Colour,
		Data_MediumP,
		Data_LowP,
		Depth
	};

private:
	std::tuple<int, int> m_dimensions;

	GLTextureType m_type;
	GLTextureFormat m_format;
	GLTextureFiltering m_filtering_min;
	GLTextureFiltering m_filtering_mag;
	TargetType m_target;
	bool m_generate_mipmaps;

	GLuint m_texture = GL_NONE;
	GLint m_preferred_format = GL_NONE;

private:
	void ConfigureTexture(bool create, std::optional<GLTextureFormat> pixel_format = std::optional<GLTextureFormat>(), std::vector<const void*> pixels = {});
	GLint GetPreferredFormat(bool force = false);

	void SetPreset(Preset preset, bool configure = true);

public:
	GLTexture(Preset preset, TargetType target, std::tuple<int, int> dimensions = std::tuple<int, int>(1, 1), bool generate_mipmaps = false);
	GLTexture(GLTextureDataPreset preset, TargetType target, bool generate_mipmaps = false);
	GLTexture(GLTexturePreset preset, bool generate_mipmaps = false);
	GLTexture(const GLTexture& copy_from);
	GLTexture& operator=(const GLTexture& copy_from);
	GLTexture(GLTexture&& move_from) noexcept;
	GLTexture& operator=(GLTexture&& move_from) noexcept;
	~GLTexture();

	void SetDimensions(std::tuple<int, int> dimensions);
	std::tuple<int, int> GetDimensions() const;

	void SetTextureType(GLTextureType type);
	GLTextureType GetTextureType() const;

	void SetFormat(GLTextureFormat format);
	GLTextureFormat GetFormat() const;

	void SetFiltering(GLTextureFiltering filtering);

	void SetMinFiltering(GLTextureFiltering min_filtering);
	GLTextureFiltering GetMinFiltering() const;

	void SetMagFiltering(GLTextureFiltering mag_filtering);
	GLTextureFiltering GetMagFiltering() const;

	TargetType GetTargetType() const;

	void SetGenerateMipMaps(bool generate_mipmaps);
	bool GetGenerateMipMaps() const;

	GLuint GetTexture() const;
	void BindTexture() const;

	void SetPixels(GLTextureFormat pixel_format, std::vector<const void*> pixels);
	void SetPixels(GLTextureDataPreset preset);

	void CopyTo(GLTexture& dest) const;
	void CopyFrom(const GLTexture& src);
};

std::optional<int> GetNumColourTextures(RenderTargetMode mode); //returning no value means this mode links into textures from another mode
int GetNumAttachedColourTextures(RenderTargetMode mode);