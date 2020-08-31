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
