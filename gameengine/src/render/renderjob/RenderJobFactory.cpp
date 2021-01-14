#include "RenderJobFactory.h"

#include "../rendertarget/RenderTarget.h"

RenderJobFactory::RenderJobFactory(Engine* engine, RenderTarget* target) : m_engine(engine), m_target(target)
{
}

RenderJobFactory::~RenderJobFactory()
{
}

Engine* RenderJobFactory::GetEngine() const
{
	return this->m_engine;
}

RenderTarget* RenderJobFactory::GetTarget() const
{
	return this->m_target;
}

std::tuple<int, int> RenderJobFactory::GetOutputSize() const
{
	return this->m_target->GetOutputSize();
}

void RenderJobFactory::CopyTo(const RenderJobFactory* dest) const
{
	dest->CopyFrom(this);
}

RenderJobInitialiser::~RenderJobInitialiser()
{
}
