#pragma once

typedef int ModelReference;
typedef int TextureReference;
typedef int RenderTextureReference;

template<class ReferenceType>
class Referenceable
{
private:
	ReferenceType m_reference;

protected:
	void SetReference(ReferenceType reference)
	{
		this->m_reference = reference;
	}

public:
	Referenceable(ReferenceType reference)
	{
		this->m_reference = reference;
	};

	ReferenceType GetReference() const
	{
		return this->m_reference;
	}
};
