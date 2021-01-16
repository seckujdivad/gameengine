#include "NormalRenderJob.h"

#include "NormalRenderJobFactory.h"
#include "../rendertarget/RenderTarget.h"

NormalRenderJob::NormalRenderJob(NormalRenderJobFactory* factory) : RenderJob(factory)
{
}

void NormalRenderJob::Render(std::vector<Model*> models, bool continuous_draw)
{
	static_cast<NormalRenderJobFactory*>(this->GetFactory())->Render(models, continuous_draw);
}
