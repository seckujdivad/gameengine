#include "CumulativeTexture.h"

#include <stdexcept>

#include "RenderTexture.h"
#include "RenderTextureData.h"
#include "../scene/Scene.h"
#include "../Engine.h"

CumulativeTexture::CumulativeTexture()
{
}

CumulativeTexture::CumulativeTexture(std::vector<RenderTexture*> textures) :
	m_textures(textures)
{
	if (textures.size() > 0U)
	{
		RenderTextureInfo info = textures.at(0)->GetTextureInfo();

		for (int i = 1; i < (int)textures.size(); i++)
		{
			if (textures.at(i)->GetTextureInfo() != info)
			{
				throw std::invalid_argument("Texture info for texture " + std::to_string(i) + " doesn't match the texture info for texture 0");
			}
		}
	}
	else
	{
		throw std::invalid_argument("At least one RenderTexture must be provided");
	}
}

void CumulativeTexture::Render(int index, bool continuous_draw) const
{
	if ((index < 0) || (index >= (int)this->m_textures.size()))
	{
		throw std::invalid_argument("\"index\" must be between 0 and " + std::to_string((int)this->m_textures.size() - 1) + " but is " + std::to_string(index));
	}

	for (int i = index; i < (int)this->m_textures.size(); i++)
	{
		if (i != 0)
		{
			CopyTextureGroup(this->m_textures.at(i - 1)->GetOutputTextures(), this->m_textures.at(i)->GetWriteTextures(), this->m_textures.at(i)->GetTextureInfo(), this->m_textures.at(i)->GetOutputSize());
		}

		this->m_textures.at(i)->Render(this->m_textures.at(i)->GetEngine()->GetScene()->GetModels(), continuous_draw);
	}
}

RenderTexture* CumulativeTexture::GetOutput() const
{
	if (this->m_textures.size() == 0U)
	{
		throw std::runtime_error("No textures stored");
	}
	else
	{
		return this->m_textures.at((int)this->m_textures.size() - 1);
	}
}
