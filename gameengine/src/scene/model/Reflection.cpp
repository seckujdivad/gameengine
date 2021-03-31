#include "Reflection.h"

Reflection::Reflection(RenderTextureReference reference) : Nameable(), Cubemap(reference)
{
}

void Reflection::SetDrawShadows(bool value)
{
	this->m_draw_shadows = value;
}

bool Reflection::GetDrawShadows() const
{
	return this->m_draw_shadows;
}

void Reflection::SetDrawReflections(bool value)
{
	this->m_draw_reflections = value;
}

bool Reflection::GetDrawReflections() const
{
	return this->m_draw_reflections;
}
