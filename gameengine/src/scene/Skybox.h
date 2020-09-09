#pragma once

#include "Cubemap.h"
#include "Nameable.h"

class Skybox : public Cubemap, public Nameable
{
private:
public:
	Skybox(RenderTextureReference reference);
};