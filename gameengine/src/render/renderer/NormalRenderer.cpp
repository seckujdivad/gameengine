#include "NormalRenderer.h"

#include <stdexcept>

#include "../rendertarget/RenderTarget.h"
#include "../rendertarget/RenderTexture.h"

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

	RenderTextureInfo info;
	info.colour = false;
	info.num_data = 0;
	info.auto_generate_textures = false;

	RenderTargetConfig config;
	config.SetMode(RenderTargetMode::Normal_DepthOnly);

	this->m_rt_depth_only = std::make_unique<RenderTexture>(-1, this->GetEngine(), config, info, this->GetTarget()->GetTargetType());
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
			this->GetDepthOnlyTarget()->CopyFrom(src_renderer->GetDepthOnlyTarget());
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
