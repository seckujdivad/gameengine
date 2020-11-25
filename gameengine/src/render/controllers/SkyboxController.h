#pragma once

#include "CubemapController.h"

class SkyboxController : public CubemapController
{
private:
	RenderTexture* GenerateRenderTexture(int layer) const override;
	bool RepeatingConfigureRenderTexture(RenderTexture* render_texture) const override;

public:
	SkyboxController(Engine* engine, RenderTextureReference reference);

	double GetRenderGroup() const override;
	RenderControllerType GetType() const override;

	CubemapType GetCubemapType() const override;
};