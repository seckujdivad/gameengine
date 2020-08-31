#pragma once

#include "../Cubemap.h"
#include "../Nameable.h"

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
	int GetIterations() const;
};