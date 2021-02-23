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
		throw std::invalid_argument("At least one renderer must be provided");
	}

	this->SetFetchModelsFunction([&renderers = this->m_renderers](int layer)
		{
			return renderers.at(layer)->GetEngine()->GetScene()->GetModels();
		});
}

void CumulativeTexture::Render(std::vector<Model*> models, bool continuous_draw)
{
	this->Render(0, continuous_draw, models);
}

void CumulativeTexture::Render(int index, bool continuous_draw, std::optional<std::vector<Model*>> models) const
{
	if (this->m_renderers.size() == 0)
	{
		throw std::invalid_argument("CumulativeTexture has no renderers");
	}

	if ((index < 0) || (index >= static_cast<int>(this->m_renderers.size())))
	{
		throw std::invalid_argument("\"index\" must be a member of Z U [0, " + std::to_string(static_cast<int>(this->m_renderers.size()) - 1) + "] but is " + std::to_string(index));
	}

	bool propagate_draws = false;
	for (int i = index; i < static_cast<int>(this->m_renderers.size()); i++)
	{
		RenderTarget* target = this->m_renderers.at(i)->GetTarget();

		std::vector<Model*> models_to_draw;
		if (models.has_value())
		{
			models_to_draw = models.value();
		}
		else
		{
			models_to_draw = this->m_fetch_models_function(i);
		}

		if (propagate_draws || models_to_draw.size() > 0 || !target->FramebufferContainsRenderOutput() || target->IsFBOClearedOnRender())
		{
			if (i != 0 && !target->IsFBOClearedOnRender())
			{
				this->m_renderers.at(i - 1)->CopyTo(this->m_renderers.at(i));
			}

			this->m_renderers.at(i)->Render(models_to_draw, continuous_draw);
			propagate_draws = true;
		}
	}
}

RenderTarget* CumulativeTexture::GetOutput() const
{
	return (*this->m_renderers.rbegin())->GetTarget();
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
