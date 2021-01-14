#pragma once

#include "../RenderMode.h"

enum class RenderJobType
{
	Wrapper,
	Normal
};

RenderJobType GetRenderJobType(RenderMode mode);