#include "Reflection.h"

Reflection::Reflection(RenderTextureReference reference) : Nameable(), Cubemap(reference)
{
}

void Reflection::SetIterations(int iterations)
{
	this->m_parallax_iterations = iterations;
}

int Reflection::GetIterations() const
{
	return this->m_parallax_iterations;
}

void Reflection::SetDrawShadows(bool value)
{
	this->m_draw_shadows = value;
}

bool Reflection::GetDrawShadows()
{
	return this->m_draw_shadows;
}
