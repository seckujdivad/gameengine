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
	constexpr void SetReference(ReferenceType reference)
	{
		this->m_reference = reference;
	}

public:
	constexpr Referenceable(ReferenceType reference) : m_reference(reference)
	{
	};

	constexpr ReferenceType GetReference() const
	{
		return this->m_reference;
	}
};
