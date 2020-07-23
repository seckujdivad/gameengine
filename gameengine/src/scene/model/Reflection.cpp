#include <wx/wxprec.h>
#include "Reflection.h"

Reflection::Reflection(CubemapReference reference) : Nameable(), Cubemap(reference)
{
}

void Reflection::SetIterations(int iterations)
{
	this->m_parallax_iterations = iterations;
}

int Reflection::GetIterations()
{
	return this->m_parallax_iterations;
}
