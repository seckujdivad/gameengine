#include "NormalRenderer.h"

#include <stdexcept>

#include "../rendertarget/target/RenderTarget.h"
#include "../rendertarget/texture/RenderTexture.h"
#include "../rendertarget/target/RenderTargetConfig.h"
#include "../rendertarget/target/RenderTargetMode.h"

NormalRenderer::NormalRenderer(Engine* engine, RenderTarget* target) : Renderer(engine, target)
{
	if (this->GetTarget()->GetRenderMode() != RenderTargetMode::Normal_Draw)
	{
		throw std::invalid_argument("Provided target must have mode Normal_Draw");
	}

	RenderTexture* target_texture = dynamic_cast<RenderTexture*>(target);

	if (target_texture == nullptr)
	{
		throw std::invalid_argument("Target must inherit from RenderTexture");
	}

	RenderTargetConfig config; 
	config.SetMode(RenderTargetMode::Normal_DepthOnly);
	config.clear_fbo = this->GetTarget()->GetConfig().clear_fbo;

	this->m_rt_depth_only = std::make_unique<RenderTexture>(-1, this->GetEngine(), config, std::optional<RenderTextureGroup*>(), true, false);
	this->m_rt_depth_only->SetWriteTarget(target_texture);
}

bool NormalRenderer::SetOutputSize(std::tuple<int, int> dimensions)
{
	return this->GetTarget()->SetOutputSize(dimensions) || this->m_rt_depth_only->SetOutputSize(dimensions);
}

void NormalRenderer::CopyFrom(const Renderer* src) const
{
	if (src != this)
	{
		const NormalRenderer* src_renderer = dynamic_cast<const NormalRenderer*>(src);

		if (src_renderer == nullptr)
		{
			throw std::invalid_argument("src must inherit from NormalRenderer");
		}
		else
		{
			this->GetDrawTarget()->CopyFrom(src_renderer->GetDrawTarget());
		}
	}
}

std::unordered_set<RenderTextureReference> NormalRenderer::GetRenderTextureDependencies() const
{
	std::unordered_set<RenderTextureReference> references = this->GetDepthOnlyTarget()->GetRenderTextureDependencies();

	std::unordered_set<RenderTextureReference> draw_references = this->GetDrawTarget()->GetRenderTextureDependencies();
	references.insert(draw_references.begin(), draw_references.end());
	return references;
}

void NormalRenderer::Render(std::vector<Model*> models, bool continuous_draw)
{
	if (this->GetDepthOnlyTarget()->GetConfig().clear_fbo != this->GetTarget()->GetConfig().clear_fbo)
	{
		RenderTargetConfig config = this->GetDepthOnlyTarget()->GetConfig();
		config.clear_fbo = this->GetTarget()->GetConfig().clear_fbo;
		this->GetDepthOnlyTarget()->SetConfig(config);
	}

	this->m_rt_depth_only->SetCamera(this->GetTarget()->GetCamera());
	this->m_rt_depth_only->SetOutputSize(this->GetTarget()->GetOutputSize());

	this->m_rt_depth_only->Render(models, continuous_draw);
	this->GetTarget()->Render(models, continuous_draw);
}

RenderTexture* NormalRenderer::GetDepthOnlyTarget() const
{
	return this->m_rt_depth_only.get();
}

RenderTexture* NormalRenderer::GetDrawTarget() const
{
	return dynamic_cast<RenderTexture*>(this->GetTarget());
}
