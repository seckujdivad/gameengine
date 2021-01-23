#pragma once

#include "RenderJobFactory.h"
#include "../rendertarget/RenderTargetConfig.h"

struct NormalRenderJobInitialiser : public RenderJobInitialiser
{
};

class NormalRenderJobFactory : public RenderJobFactory
{
private:
	std::unique_ptr<NormalRenderJobInitialiser> m_default_initialiser;

public:
	NormalRenderJobFactory(Engine* engine, RenderTarget* target);

	bool SetOutputSize(std::tuple<int, int> dimensions) override;

	std::shared_ptr<RenderJob> GenerateJob(RenderJobInitialiser* initialiser) override;

	void CopyFrom(const RenderJobFactory* src) const override;

	std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const override;

	NormalRenderJobInitialiser& GetDefaultInitialiser();
};