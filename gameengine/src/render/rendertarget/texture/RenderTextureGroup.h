#pragma once

#include <tuple>
#include <vector>
#include <optional>
#include <functional>

#include "../../gltexture/GLTexture.h"

enum class TargetType;
enum class RenderTargetMode;

class RenderTextureGroup
{
public:
	struct Identifiers
	{
		std::vector<GLuint> colour;
		std::optional<GLuint> depth;
	};

private:
	const GLTexture& GetATexture() const;

public:
	RenderTextureGroup();
	RenderTextureGroup(RenderTargetMode mode, TargetType target);

	std::vector<std::shared_ptr<GLTexture>> colour;
	std::optional<std::shared_ptr<GLTexture>> depth;

	std::tuple<int, int> GetDimensions() const;
	bool SetDimensions(std::tuple<int, int> dimensions);

	TargetType GetTargetType() const;

	void CopyFrom(const RenderTextureGroup& src, bool deep_copy_textures = false);
	void CopyTo(RenderTextureGroup& dest, bool deep_copy_textures = false) const;

	void ForEachTexture(std::function<void(std::shared_ptr<GLTexture>& texture)> foreach);

	Identifiers GetIdentifiers() const;

	void AttachToFBO(GLuint fbo) const;
};