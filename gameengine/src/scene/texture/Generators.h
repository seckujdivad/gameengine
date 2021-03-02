#pragma once

#include <tuple>

#include "Texture.h"

enum class XORType
{
	Greyscale,
	HSV
};

void GenerateXORTexture(Texture& texture, std::tuple<int, int> dimensions, XORType type);