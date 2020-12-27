#pragma once

#include "../Cubemap.h"
#include "../Nameable.h"

enum class ReflectionMode //reproduced in resources/shaders/model_normal.frag
{
	Iterative,
	OBB
};

class Reflection : public Nameable, public Cubemap
{
private:
	//iterative parallax correction
	int m_parallax_iterations = 1;

	bool m_draw_shadows = true;
	bool m_draw_reflections = true;

public:
	Reflection(RenderTextureReference reference);

	//iterative mode only
	void SetIterations(int iterations);
	int GetIterations() const;

	void SetDrawShadows(bool value);
	bool GetDrawShadows() const;

	void SetDrawReflections(bool value);
	bool GetDrawReflections() const;
};