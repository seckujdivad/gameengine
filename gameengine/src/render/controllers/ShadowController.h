#pragma once

#include "CubemapController.h"

class ShadowController : public CubemapController
{
private:
	RenderTextureInfo GetRenderTextureInfo() const override;
	RenderMode GetRenderMode() const override;
	void InitialConfigureRenderTexture(RenderTexture* render_texture) const override;
	bool RepeatingConfigureRenderTexture(RenderTexture* render_texture) const override;

public:
	ShadowController(Engine* engine, RenderTextureReference reference);

	double GetRenderGroup() const override;
	RenderControllerType GetType() const override;

	CubemapType GetCubemapType() const override;
};