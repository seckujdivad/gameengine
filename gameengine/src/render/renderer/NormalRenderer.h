#pragma once

#include <memory>

#include "Renderer.h"
#include "../rendertarget/RenderTargetConfig.h"

class RenderTexture;

class NormalRenderer : public Renderer
{
private:
	std::unique_ptr<RenderTexture> m_rt_depth_only;

public:
	NormalRenderer(Engine* engine, RenderTarget* target);

	void CopyFrom(const Renderer* src) const override;

	std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const override;

	bool SetOutputSize(std::tuple<int, int> dimensions) override;
	void Render(std::vector<Model*> models, bool continuous_draw = false) override;

	RenderTexture* GetDepthOnlyTarget() const;
	RenderTexture* GetDrawTarget() const;
};