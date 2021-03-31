#pragma once

#include "../Cubemap.h"
#include "../Nameable.h"

enum class ReflectionMode //reproduced in resources/shaders/model_normal.frag
{
	Simple,
	OBB
};

class Reflection : public Nameable, public Cubemap
{
private:
	bool m_draw_shadows = true;
	bool m_draw_reflections = true;

public:
	Reflection(RenderTextureReference reference);

	void SetDrawShadows(bool value);
	bool GetDrawShadows() const;

	void SetDrawReflections(bool value);
	bool GetDrawReflections() const;
};