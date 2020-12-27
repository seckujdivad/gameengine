#pragma once

#include "CubemapController.h"

class ReflectionController : public CubemapController
{
private:
	RenderTexture* GenerateRenderTexture(int layer) const override;
	bool RepeatingConfigureRenderTexture(RenderTexture* render_texture) const override;

public:
	ReflectionController(Engine* engine, RenderTextureReference reference);

	void PostRender() override;

	RenderControllerType GetType() const override;

	CubemapType GetCubemapType() const override;
};