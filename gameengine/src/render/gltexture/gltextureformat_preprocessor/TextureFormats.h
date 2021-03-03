#pragma once

#include "../../../GLComponents.h"

enum class GLTextureType;

//do not use this function outside a wrapper
GLint GetTextureFormatEnum(int num_channels, GLTextureType type, int bit_depth);