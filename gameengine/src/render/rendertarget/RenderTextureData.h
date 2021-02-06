#pragma once

#include <tuple>
#include <vector>
#include <optional>
#include <functional>

#include "../texture/Texture.h"

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
	const Texture& GetATexture() const;

public:
	RenderTextureGroup();
	RenderTextureGroup(RenderTargetMode mode, TargetType target);

	std::vector<Texture> colour;
	std::optional<Texture> depth;

	std::tuple<int, int> GetDimensions() const;
	void SetDimensions(std::tuple<int, int> dimensions);

	TargetType GetTargetType() const;

	void CopyFrom(const RenderTextureGroup& src);
	void CopyTo(RenderTextureGroup& dest) const;

	void ForEachTexture(std::function<void(Texture& texture)> foreach);

	Identifiers GetIdentifiers() const;
};