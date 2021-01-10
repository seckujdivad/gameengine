#include "RenderJobFactory.h"

RenderJobFactory::RenderJobFactory(Engine* engine) : m_engine(engine)
{
}

RenderJobFactory::~RenderJobFactory()
{
}

Engine* RenderJobFactory::GetEngine() const
{
	return this->m_engine;
}
