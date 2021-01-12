#include "RenderJob.h"

#include "RenderJobFactory.h"

RenderJob::RenderJob(RenderJobFactory* factory) : Renderable(), m_factory(factory)
{
}

RenderJob::~RenderJob()
{
}

RenderJobFactory* RenderJob::GetFactory() const
{
	return this->m_factory;
}

std::tuple<int, int> RenderJob::GetOutputSize() const
{
	return this->GetFactory()->GetOutputSize();
}

bool RenderJob::SetOutputSize(std::tuple<int, int> dimensions)
{
	return this->GetFactory()->SetOutputSize(dimensions);
}
