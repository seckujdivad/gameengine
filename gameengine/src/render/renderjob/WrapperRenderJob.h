#pragma once

#include "RenderJob.h"

class WrapperRenderJobFactory;

class WrapperRenderJob : public RenderJob
{
private:
public:
	WrapperRenderJob(WrapperRenderJobFactory* factory);

	void Render(std::vector<Model*> models, bool continuous_draw = false) override;
};