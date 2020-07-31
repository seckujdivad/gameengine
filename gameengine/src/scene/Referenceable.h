#pragma once

typedef int ModelReference;
typedef int CameraReference;
typedef int TextureReference;
typedef int RenderTextureReference;
typedef int CubemapReference;

template<class ReferenceType>
class Referenceable
{
private:
	ReferenceType m_reference;

public:
	Referenceable(ReferenceType reference);

	ReferenceType GetReference();
};
