#pragma once

#include "Renderer.h"
#include "../rendertarget/RenderTargetConfig.h"

class NormalRenderer : public Renderer
{
public:
	NormalRenderer(Engine* engine, RenderTarget* target);

	bool SetOutputSize(std::tuple<int, int> dimensions) override;

	void CopyFrom(const Renderer* src) const override;

	std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const override;

	void Render(std::vector<Model*> models, bool continuous_draw = false) override;
};