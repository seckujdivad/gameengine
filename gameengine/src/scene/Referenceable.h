#pragma once

typedef unsigned int ModelReference;
typedef unsigned int CameraReference;
typedef unsigned int TextureReference;
typedef unsigned int RenderTextureReference;
typedef unsigned int CubemapReference;

template<class ReferenceType>
class Referenceable
{
private:
	ReferenceType m_reference;

public:
	Referenceable(ReferenceType reference);

	ReferenceType GetReference();
};
