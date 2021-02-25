#include "NormalRenderer.h"

#include <stdexcept>

#include "../rendertarget/target/RenderTarget.h"
#include "../rendertarget/target/RenderTargetConfig.h"
#include "../rendertarget/target/RenderTargetMode.h"
#include "../rendertarget/texture/RenderTexture.h"
#include "../rendertarget/texture/RenderTextureGroup.h"

NormalRenderer::NormalRenderer(Engine* engine, RenderTarget* target) : Renderer(engine, target)
{
	if (this->GetTarget()->GetRenderMode() != RenderTargetMode::Normal_PostProcess)
	{
		throw std::invalid_argument("Provided target must have mode Normal_PostProcess");
	}

	RenderTexture* target_texture = dynamic_cast<RenderTexture*>(target);

	if (target_texture == nullptr)
	{
		throw std::invalid_argument("Target must inherit from RenderTexture");
	}

	{
		RenderTargetConfig config;
		config.SetMode(RenderTargetMode::Normal_DepthOnly);
		config.clear_fbo = this->GetTarget()->GetConfig().clear_fbo;

		std::unique_ptr<RenderTextureGroup> textures = std::make_unique<RenderTextureGroup>(RenderTargetMode::Normal_DepthOnly, target->GetTargetType());

		this->m_rt_depth_only = std::make_unique<RenderTexture>(-1, this->GetEngine(), config, textures.get());
	}

	{
		RenderTargetConfig config;
		config.SetMode(RenderTargetMode::Normal_Draw);
		config.clear_fbo = this->GetTarget()->GetConfig().clear_fbo;
		std::get<RenderTargetConfig::Normal_Draw>(config.mode_data).depth_frame = this->GetDepthOnlyTarget()->GetOutputTextures();

		std::unique_ptr<RenderTextureGroup> textures = std::make_unique<RenderTextureGroup>(RenderTargetMode::Normal_Draw, target->GetTargetType());

		this->m_rt_draw = std::make_unique<RenderTexture>(-1, this->GetEngine(), config, textures.get());
	}

	{
		RenderTargetConfig config = this->GetTarget()->GetConfig();
		std::get<RenderTargetConfig::Normal_PostProcess>(config.mode_data).draw_frame = this->GetDrawTarget()->GetOutputTextures();
		this->GetTarget()->SetConfig(config);
	}
}

bool NormalRenderer::SetOutputSize(std::tuple<int, int> dimensions)
{
	this->m_rt_depth_only->SetOutputSize(dimensions);
	this->m_rt_draw->SetOutputSize(dimensions);
	return this->GetTarget()->SetOutputSize(dimensions);
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
			this->GetTarget()->CopyFrom(src_renderer->GetTarget());
		}
	}
}

std::unordered_set<RenderTextureReference> NormalRenderer::GetRenderTextureDependencies() const
{
	std::unordered_set<RenderTextureReference> references = this->GetDepthOnlyTarget()->GetRenderTextureDependencies();

	{
		std::unordered_set<RenderTextureReference> draw_references = this->GetDrawTarget()->GetRenderTextureDependencies();
		references.insert(draw_references.begin(), draw_references.end());
	}

	{
		std::unordered_set<RenderTextureReference> postprocess_references = this->GetTarget()->GetRenderTextureDependencies();
		references.insert(postprocess_references.begin(), postprocess_references.end());
	}

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

	if (this->GetDrawTarget()->GetConfig().clear_fbo != this->GetTarget()->GetConfig().clear_fbo)
	{
		RenderTargetConfig config = this->GetDrawTarget()->GetConfig();
		config.clear_fbo = this->GetTarget()->GetConfig().clear_fbo;
		this->GetDrawTarget()->SetConfig(config);
	}

	this->m_rt_draw->SetCamera(this->GetTarget()->GetCamera());
	this->m_rt_draw->SetOutputSize(this->GetTarget()->GetOutputSize());

	this->m_rt_depth_only->Render(models, continuous_draw);
	this->m_rt_depth_only->GetWriteTextures()->depth.value().CopyTo(this->m_rt_draw->GetWriteTextures()->depth.value());
	this->m_rt_draw->Render(models, continuous_draw);
	this->GetTarget()->Render(std::vector<Model*>(), continuous_draw);
}

RenderTexture* NormalRenderer::GetDepthOnlyTarget() const
{
	return this->m_rt_depth_only.get();
}

RenderTexture* NormalRenderer::GetDrawTarget() const
{
	return this->m_rt_draw.get();
}
