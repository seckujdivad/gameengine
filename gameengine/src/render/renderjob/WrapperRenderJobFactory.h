#pragma once

#include "RenderJobFactory.h"
#include "../rendertarget/RenderTargetConfig.h"

struct WrapperRenderJobInitialiser : public RenderJobInitialiser
{
};

class WrapperRenderJobFactory : public RenderJobFactory
{
private:
public:
	WrapperRenderJobFactory(Engine* engine, RenderTarget* target);

	bool SetOutputSize(std::tuple<int, int> dimensions) override;

	std::shared_ptr<RenderJob> GenerateJob(RenderJobInitialiser* initialiser) override;

	void CopyFrom(const RenderJobFactory* src) const override;

	std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const override;
};