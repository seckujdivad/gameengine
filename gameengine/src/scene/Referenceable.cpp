#include "Referenceable.h"

template<class ReferenceType>
Referenceable<ReferenceType>::Referenceable(ReferenceType reference)
{
	this->m_reference = reference;
}

template<class ReferenceType>
ReferenceType Referenceable<ReferenceType>::GetReference()
{
	return this->m_reference;
}

template class Referenceable<ModelReference>;;
template class Referenceable<TextureReference>;
template class Referenceable<RenderTextureReference>;