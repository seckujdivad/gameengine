#pragma once

#include <tuple>
#include <vector>
#include <optional>

#include "../../GLComponents.h"

#include "GLTexturePreset.h"
#include "GLTextureFormat.h"

#include "../GLObjectLabelable.h"

enum class GLTextureType;
enum class TargetType;
enum class GLTextureDataPreset;

enum class TextureFiltering;

enum class RenderTargetMode;

class GLTexture : public GLObjectLabelable
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
	TextureFiltering m_filtering_min;
	TextureFiltering m_filtering_mag;
	TargetType m_target;
	bool m_generate_mipmaps;

	GLuint m_texture = GL_NONE;

	std::optional<GLint> m_shadow_compare_func = std::optional<GLint>();

private:
	void ConfigureTexture(bool create, std::optional<GLTextureFormat> pixel_format = std::optional<GLTextureFormat>(), std::vector<const void*> pixels = {});

	void SetPreset(Preset preset, std::optional<int> num_channels = std::optional<int>(), bool configure = true);

public:
	GLTexture(Preset preset, TargetType target, std::optional<int> num_channels = std::optional<int>(), std::tuple<int, int> dimensions = std::tuple<int, int>(1, 1), bool generate_mipmaps = false);
	GLTexture(GLTextureDataPreset preset, TargetType target, std::optional<int> num_channels = std::optional<int>(), bool generate_mipmaps = false);
	GLTexture(GLTexturePreset preset, bool generate_mipmaps = false);
	GLTexture(const GLTexture& copy_from);
	GLTexture& operator=(const GLTexture& copy_from);
	GLTexture(GLTexture&& move_from) noexcept;
	GLTexture& operator=(GLTexture&& move_from) noexcept;
	~GLTexture();

	bool SetDimensions(std::tuple<int, int> dimensions);
	std::tuple<int, int> GetDimensions() const;

	void SetTextureType(GLTextureType type);
	GLTextureType GetTextureType() const;

	void SetFormat(GLTextureFormat format);
	GLTextureFormat GetFormat() const;

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

	void SetPixels(GLTextureFormat pixel_format, std::vector<const void*> pixels);
	void SetPixels(GLTextureDataPreset preset);

	void SetShadowCompareFunc(GLint func);
	void DisableShadowComparisons();
	bool ShadowComparisonsAreEnabled() const;
	std::optional<GLint> GetShadowCompareFunc() const;

	void CopyTo(GLTexture& dest) const;
	void CopyFrom(const GLTexture& src);

	//only to be used by features that aren't exposed through other methods
	void SetTexParameter(GLenum pname, GLint param);
	void SetTexParameter(GLenum pname, std::vector<GLint> params);
	void SetTexParameter(GLenum pname, GLfloat param);
	void SetTexParameter(GLenum pname, std::vector<GLfloat> params);
	std::vector<GLint> GetTexParameteriv(GLenum pname, std::size_t num_params) const;
	std::vector<GLfloat> GetTexParameterfv(GLenum pname, std::size_t num_params) const;
	GLint GetTexParameteri(GLenum pname) const;
	GLfloat GetTexParameterf(GLenum pname) const;
};

std::optional<int> GetNumColourTextures(RenderTargetMode mode); //returning no value means this mode links into textures from another mode
int GetNumAttachedColourTextures(RenderTargetMode mode);