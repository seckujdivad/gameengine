#pragma once

#include "CubemapController.h"

class ShadowController : public CubemapController
{
private:
	std::unique_ptr<Renderer> GenerateRenderer(int layer) override;
	bool RepeatingConfigureRenderer(Renderer* renderer) const override;

public:
	ShadowController(Engine* engine, RenderTextureReference reference);

	RenderControllerType GetType() const override;

	std::shared_ptr<Cubemap> GetTargetCubemap() const override;
};