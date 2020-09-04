#pragma once

#include "CubemapController.h"

class ReflectionController : public CubemapController
{
private:
	RenderTextureInfo GetRenderTextureInfo() const override;
	RenderMode GetRenderMode() const override;
	void InitialConfigureRenderTexture(RenderTexture* render_texture) const override;
	bool RepeatingConfigureRenderTexture(RenderTexture* render_texture) const override;

public:
	ReflectionController(Engine* engine, RenderTextureReference reference);
	
	double GetRenderGroup() const override;
	RenderControllerType GetType() const override;

	CubemapType GetCubemapType() const override;
};