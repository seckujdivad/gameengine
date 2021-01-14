#pragma once

#include "CubemapController.h"

class ShadowController : public CubemapController
{
private:
	std::unique_ptr<RenderJobFactory> GenerateFactory(int layer) override;
	bool RepeatingConfigureFactory(RenderJobFactory* factory) const override;

public:
	ShadowController(Engine* engine, RenderTextureReference reference);

	RenderControllerType GetType() const override;

	CubemapType GetCubemapType() const override;
};