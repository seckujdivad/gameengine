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

	CubemapType GetCubemapType() const override;
};