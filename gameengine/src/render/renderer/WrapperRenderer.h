#pragma once

#include "Renderer.h"
#include "../rendertarget/RenderTargetConfig.h"

class WrapperRenderer : public Renderer
{
public:
	WrapperRenderer(Engine* engine, RenderTarget* target);

	bool SetOutputSize(std::tuple<int, int> dimensions) override;

	void CopyFrom(const Renderer* src) const override;

	std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const override;

	void Render(std::vector<Model*> models, bool continuous_draw = false) override;
};