#pragma once

using ModelReference = int;
using TextureReference = int;
using RenderTextureReference = int;

constexpr ModelReference NullModelReference = -1;
constexpr TextureReference NullTextureReference = -1;
constexpr RenderTextureReference NullRenderTextureReference = -1;

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
