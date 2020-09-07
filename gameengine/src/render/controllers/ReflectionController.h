#pragma once

#include "CubemapController.h"

class ReflectionController : public CubemapController
{
private:
	RenderMode GetRenderMode() const override;
	RenderTexture* GenerateRenderTexture(int layer) const override;
	bool RepeatingConfigureRenderTexture(RenderTexture* render_texture) const override;

public:
	ReflectionController(Engine* engine, RenderTextureReference reference);

	void PostRender() override;
	
	double GetRenderGroup() const override;
	RenderControllerType GetType() const override;

	CubemapType GetCubemapType() const override;
};