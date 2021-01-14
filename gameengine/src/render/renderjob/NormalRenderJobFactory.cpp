#include "NormalRenderJobFactory.h"

#include <stdexcept>

#include "NormalRenderJob.h"
#include "../rendertarget/RenderTarget.h"

NormalRenderJobFactory::NormalRenderJobFactory(Engine* engine, RenderTarget* target) : RenderJobFactory(engine, target)
{
	this->m_default_initialiser = std::make_unique<NormalRenderJobInitialiser>();
}

bool NormalRenderJobFactory::SetOutputSize(std::tuple<int, int> dimensions)
{
	return this->GetTarget()->SetOutputSize(dimensions);
}

std::shared_ptr<RenderJob> NormalRenderJobFactory::GenerateJob(RenderJobInitialiser* initialiser)
{
	NormalRenderJobInitialiser* init;
	if (initialiser == nullptr)
	{
		init = this->m_default_initialiser.get();
	}
	else
	{
		init = dynamic_cast<NormalRenderJobInitialiser*>(initialiser);
	}

	if (init == nullptr)
	{
		throw std::invalid_argument("Initialiser must be castable to NormalRenderJobInitialiser as this is a NormalRenderJobFactory");
	}
	else
	{
		std::shared_ptr<NormalRenderJob> job = std::make_shared<NormalRenderJob>(this);
		return job;
	}
}

void NormalRenderJobFactory::CopyFrom(const RenderJobFactory* src) const
{
	if (src != this)
	{
		this->GetTarget()->CopyFrom(src->GetTarget());
	}
}

std::unordered_set<RenderTextureReference> NormalRenderJobFactory::GetRenderTextureDependencies() const
{
	return this->GetTarget()->GetRenderTextureDependencies();
}

NormalRenderJobInitialiser& NormalRenderJobFactory::GetDefaultInitialiser()
{
	return *this->m_default_initialiser;
}
