#pragma once

#include "CubemapController.h"

class ReflectionController : public CubemapController
{
private:
	std::unique_ptr<RenderJobFactory> GenerateFactory(int layer) override;
	bool RepeatingConfigureFactory(RenderJobFactory* factory) const override;

public:
	ReflectionController(Engine* engine, RenderTextureReference reference);

	void PostRender() override;

	RenderControllerType GetType() const override;

	CubemapType GetCubemapType() const override;
};