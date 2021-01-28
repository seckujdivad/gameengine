#pragma once

#include "CubemapController.h"

class ReflectionController : public CubemapController
{
private:
	std::unique_ptr<Renderer> GenerateRenderer(int layer) override;
	bool RepeatingConfigureRenderer(Renderer* renderer) const override;

public:
	ReflectionController(Engine* engine, RenderTextureReference reference);

	RenderControllerType GetType() const override;

	CubemapType GetCubemapType() const override;
};