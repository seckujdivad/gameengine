#pragma once

#include "../GLComponents.h"

#include <string>

enum class TargetType
{
	Texture_2D,
	Texture_Cubemap
};

TargetType GetTargetType(GLenum target);
GLenum GetTargetEnum(TargetType target);

int GetNumTextures(TargetType target);

std::string TargetTypeToString(TargetType target);