#pragma once

using ModelReference = int;
using TextureReference = int;
using RenderTextureReference = int;

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
