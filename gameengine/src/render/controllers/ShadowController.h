#pragma once

#include "CubemapController.h"

class ShadowController : public CubemapController
{
private:
	RenderTexture* GenerateRenderTexture(int layer) const override;
	bool RepeatingConfigureRenderTexture(RenderTexture* render_texture) const override;

public:
	ShadowController(Engine* engine, RenderTextureReference reference);

	RenderControllerType GetType() const override;

	CubemapType GetCubemapType() const override;
};