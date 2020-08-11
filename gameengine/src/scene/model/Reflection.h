#pragma once

#include <wx/wxprec.h>
#include "../../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <vector>
#include <tuple>

#include "../Nameable.h"
#include "../Cubemap.h"
#include "../Referenceable.h"

enum class ReflectionMode
{
	Iterative,
	OBB
};

class Reflection : public Nameable, public Cubemap
{
private:
	//iterative parallax correction
	int m_parallax_iterations = 1;

public:
	Reflection(RenderTextureReference reference);

	//iterative mode only
	void SetIterations(int iterations);
	int GetIterations();
};