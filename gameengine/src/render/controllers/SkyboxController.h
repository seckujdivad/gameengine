#pragma once

#include "CubemapController.h"

class SkyboxController : public CubemapController
{
private:
	std::unique_ptr<RenderJobFactory> GenerateFactory(int layer) override;
	bool RepeatingConfigureFactory(RenderJobFactory* factory) const override;

public:
	SkyboxController(Engine* engine, RenderTextureReference reference);

	RenderControllerType GetType() const override;

	CubemapType GetCubemapType() const override;
};