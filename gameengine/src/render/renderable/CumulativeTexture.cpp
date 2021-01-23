#include "CumulativeTexture.h"

#include <stdexcept>

#include "../rendertarget/RenderTexture.h"
#include "../rendertarget/RenderTextureData.h"
#include "../../scene/Scene.h"
#include "../../Engine.h"
#include "../renderer/Renderer.h"

CumulativeTexture::CumulativeTexture(std::vector<Renderer*> renderers) : m_renderers(renderers)
{
	if (renderers.size() == 0)
	{
		throw std::invalid_argument("At least one factory must be provided");
	}

	this->SetFetchModelsFunction([&renderers = this->m_renderers](int layer)
		{
			return renderers.at(layer)->GetEngine()->GetScene()->GetModels();
		});
}

void CumulativeTexture::Render(std::vector<Model*> models, bool continuous_draw)
{
	this->Render(0, continuous_draw);
}

void CumulativeTexture::Render(int index, bool continuous_draw) const
{
	if ((index < 0) || (index >= static_cast<int>(this->m_renderers.size())))
	{
		throw std::invalid_argument("\"index\" must be between 0 and " + std::to_string(static_cast<int>(this->m_renderers.size()) - 1) + " but is " + std::to_string(index));
	}

	for (int i = index; i < static_cast<int>(this->m_renderers.size()); i++)
	{
		if (i != 0)
		{
			this->m_renderers.at(i - 1)->CopyTo(this->m_renderers.at(i));
		}

		this->m_renderers.at(i)->Render(this->m_fetch_models_function(i), continuous_draw);
	}
}

RenderTarget* CumulativeTexture::GetOutput() const
{
	return this->m_renderers.at(this->m_renderers.size() - 1)->GetTarget();
}

std::tuple<int, int> CumulativeTexture::GetOutputSize() const
{
	return this->m_renderers.at(0)->GetOutputSize();
}

bool CumulativeTexture::SetOutputSize(std::tuple<int, int> dimensions)
{
	bool result = false;
	for (Renderer* renderer : this->m_renderers)
	{
		result = result || renderer->SetOutputSize(dimensions);
	}
	return result;
}

void CumulativeTexture::SetFetchModelsFunction(FetchModelsFunction func)
{
	this->m_fetch_models_function = func;
}
