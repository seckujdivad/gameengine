#include "CumulativeTexture.h"

#include <stdexcept>

#include "rendertarget/RenderTexture.h"
#include "rendertarget/RenderTextureData.h"
#include "../scene/Scene.h"
#include "../Engine.h"
#include "renderjob/RenderJobFactory.h"

CumulativeTexture::CumulativeTexture(std::vector<RenderJobFactory*> factories) : m_factories(factories)
{
	if (factories.size() == 0)
	{
		throw std::invalid_argument("At least one factory must be provided");
	}

	this->SetFetchModelsFunction([&factories = this->m_factories](int layer)
		{
			return factories.at(layer)->GetEngine()->GetScene()->GetModels();
		});
}

void CumulativeTexture::Render(int index, bool continuous_draw) const
{
	if ((index < 0) || (index >= static_cast<int>(this->m_factories.size())))
	{
		throw std::invalid_argument("\"index\" must be between 0 and " + std::to_string(static_cast<int>(this->m_factories.size()) - 1) + " but is " + std::to_string(index));
	}

	for (int i = index; i < static_cast<int>(this->m_factories.size()); i++)
	{
		if (i != 0)
		{
			this->m_factories.at(i - 1)->CopyTo(this->m_factories.at(i));
		}

		this->m_factories.at(i)->GenerateJob(nullptr)->Render(this->m_fetch_models_function(i), continuous_draw);
	}
}

RenderTarget* CumulativeTexture::GetOutput() const
{
	return this->m_factories.at(this->m_factories.size() - 1)->GetTarget();
}

void CumulativeTexture::SetFetchModelsFunction(FetchModelsFunction func)
{
	this->m_fetch_models_function = func;
}
