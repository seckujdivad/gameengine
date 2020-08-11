#pragma once

typedef int ModelReference;
typedef int TextureReference;
typedef int RenderTextureReference;

template<class ReferenceType>
class Referenceable
{
private:
	ReferenceType m_reference;

public:
	Referenceable(ReferenceType reference);

	ReferenceType GetReference();
};
