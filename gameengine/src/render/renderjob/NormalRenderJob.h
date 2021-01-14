#pragma once

#include "RenderJob.h"

class NormalRenderJobFactory;

class NormalRenderJob : public RenderJob
{
private:
public:
	NormalRenderJob(NormalRenderJobFactory* factory);

	void Render(std::vector<Model*> models, bool continuous_draw = false) override;
};