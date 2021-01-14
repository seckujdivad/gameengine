#include "WrapperRenderJobFactory.h"

#include <stdexcept>

#include "WrapperRenderJob.h"
#include "../rendertarget/RenderTarget.h"

WrapperRenderJobFactory::WrapperRenderJobFactory(Engine* engine, RenderTarget* target) : RenderJobFactory(engine, target)
{
}

bool WrapperRenderJobFactory::SetOutputSize(std::tuple<int, int> dimensions)
{
	return this->GetTarget()->SetOutputSize(dimensions);
}

std::shared_ptr<RenderJob> WrapperRenderJobFactory::GenerateJob(RenderJobInitialiser* initialiser)
{
	std::unique_ptr<WrapperRenderJobInitialiser> default_initialiser;
	WrapperRenderJobInitialiser* init;
	if (initialiser == nullptr)
	{
		default_initialiser = std::make_unique<WrapperRenderJobInitialiser>();
		init = default_initialiser.get();
	}
	else
	{
		init = dynamic_cast<WrapperRenderJobInitialiser*>(initialiser);
	}
	
	if (init == nullptr)
	{
		throw std::invalid_argument("Initialiser must be castable to WrapperRenderJobInitialiser as this is a WrapperRenderJobFactory");
	}
	else
	{
		std::shared_ptr<WrapperRenderJob> job = std::make_shared<WrapperRenderJob>(this);

		return job;
	}
}

void WrapperRenderJobFactory::CopyFrom(const RenderJobFactory* src) const
{
	if (src != this)
	{
		this->GetTarget()->CopyFrom(src->GetTarget());
	}
}

std::unordered_set<RenderTextureReference> WrapperRenderJobFactory::GetRenderTextureDependencies() const
{
	return this->GetTarget()->GetRenderTextureDependencies();
}
