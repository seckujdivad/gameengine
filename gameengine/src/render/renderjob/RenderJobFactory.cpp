#include "RenderJobFactory.h"

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
