#include "WrapperRenderJob.h"

#include "WrapperRenderJobFactory.h"
#include "../rendertarget/RenderTarget.h"

WrapperRenderJob::WrapperRenderJob(WrapperRenderJobFactory* factory) : RenderJob(factory)
{
}

void WrapperRenderJob::Render(std::vector<Model*> models, bool continuous_draw)
{
	this->GetFactory()->GetTarget()->Render(models, continuous_draw);
}
