#pragma once

#include "../RenderMode.h"

enum class RendererType
{
	Wrapper,
	Normal
};

RendererType GetRendererType(RenderMode mode);