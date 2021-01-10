#include "RenderJob.h"

RenderJob::RenderJob(RenderJobFactory* factory) : m_factory(factory)
{
}

RenderJob::~RenderJob()
{
}

RenderJobFactory* RenderJob::GetFactory() const
{
	return this->m_factory;
}
