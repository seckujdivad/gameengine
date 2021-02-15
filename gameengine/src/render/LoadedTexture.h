#pragma once

#include <string>
#include <memory>

#include "../GLComponents.h"

#include "texture/Texture.h"
#include "TargetType.h"

struct LoadedTexture
{
	inline LoadedTexture() {};
	inline LoadedTexture(std::shared_ptr<Texture> texture)
	{
		*this = texture;
	};

	inline LoadedTexture& operator=(const std::shared_ptr<Texture>& texture)
	{
		this->id = texture->GetTexture();
		this->type = GetTargetEnum(texture->GetTargetType());

		return *this;
	}

	GLuint id = NULL;
	GLenum type = NULL;
	std::string uniform_name;
};