#pragma once

#include "CubemapController.h"

class ShadowController : public CubemapController
{
private:
	RenderMode GetRenderMode() const override;
	RenderTexture* GenerateRenderTexture(int layer) const override;
	bool RepeatingConfigureRenderTexture(RenderTexture* render_texture) const override;

public:
	ShadowController(Engine* engine, RenderTextureReference reference);

	double GetRenderGroup() const override;
	RenderControllerType GetType() const override;

	CubemapType GetCubemapType() const override;
};