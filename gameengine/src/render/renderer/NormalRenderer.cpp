#include "NormalRenderer.h"

#include <stdexcept>

#include "../rendertarget/target/RenderTarget.h"
#include "../rendertarget/target/RenderTargetConfig.h"
#include "../rendertarget/target/RenderTargetMode.h"
#include "../rendertarget/texture/RenderTexture.h"
#include "../rendertarget/texture/RenderTextureGroup.h"

#include "../../scene/Camera.h"

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

		std::shared_ptr<RenderTextureGroup> textures = std::make_shared<RenderTextureGroup>(RenderTargetMode::Normal_DepthOnly, target->GetTargetType());

		this->m_rt_depth_only = std::make_unique<RenderTexture>(-1, this->GetEngine(), config, textures);
	}

	{
		RenderTargetConfig config;
		config.SetMode(RenderTargetMode::Normal_Draw);
		config.clear_fbo = this->GetTarget()->GetConfig().clear_fbo;
		std::get<RenderTargetConfig::Normal_Draw>(config.mode_data).depth_frame = this->GetDepthOnlyTarget()->GetOutputTextures();

		std::shared_ptr<RenderTextureGroup> textures = std::make_shared<RenderTextureGroup>(RenderTargetMode::Normal_Draw, target->GetTargetType());

		this->m_rt_draw = std::make_unique<RenderTexture>(-1, this->GetEngine(), config, textures);
	}

	{
		RenderTargetConfig config;
		config.SetMode(RenderTargetMode::Normal_SSRQuality);
		std::get<RenderTargetConfig::Normal_SSRQuality>(config.mode_data).draw_frame = this->GetDrawTarget()->GetOutputTextures();

		std::shared_ptr<RenderTextureGroup> textures = std::make_unique<RenderTextureGroup>(RenderTargetMode::Normal_SSRQuality, target->GetTargetType());

		this->m_rt_ssrquality = std::make_unique<RenderTexture>(-1, this->GetEngine(), config, textures);
	}

	{
		for (int i = 0; i < 2; i++)
		{
			RenderTargetConfig config = RenderTargetConfig(RenderTargetMode::PostProcess);
			config.Data<RenderTargetConfig::PostProcess>().data = RenderTargetConfig::PostProcess::BoxBlur();
			config.Data<RenderTargetConfig::PostProcess>().Data<RenderTargetConfig::PostProcess::BoxBlur>().is_first_pass = i == 0;
			config.Data<RenderTargetConfig::PostProcess>().Data<RenderTargetConfig::PostProcess::BoxBlur>().radius = glm::ivec2(1);


			RenderTargetConfig::PostProcess::Layer layer;
			if (i == 0)
			{
				layer.texture = &this->m_rt_ssrquality->GetOutputTextures()->colour.at(0);
			}
			else if (i == 1)
			{
				layer.texture = &this->GetSSRBoxBlurTarget(0)->GetOutputTextures()->colour.at(0);
			}

			config.Data<RenderTargetConfig::PostProcess>().layers.push_back(layer);

			std::shared_ptr<RenderTextureGroup> target_textures = nullptr;
			if (i != -1)
			{
				target_textures = std::make_shared<RenderTextureGroup>(RenderTargetMode::Normal_SSRQuality, target->GetTargetType());
			}
			else if (i == 1)
			{
				target_textures = this->m_rt_ssrquality->GetOutputTextures();
			}

			this->m_rt_ssrquality_boxblur.at(i) = std::make_unique<RenderTexture>(-1, this->GetEngine(), config, target_textures);
		}
	}

	{
		RenderTargetConfig config = this->GetTarget()->GetConfig();
		std::get<RenderTargetConfig::Normal_PostProcess>(config.mode_data).draw_frame = this->GetDrawTarget()->GetOutputTextures();
		this->GetTarget()->SetConfig(config);
	}

	{
		RenderTargetConfig config = this->GetDrawTarget()->GetConfig();
		std::get<RenderTargetConfig::Normal_Draw>(config.mode_data).ssr_quality_frame = this->GetSSRBoxBlurTarget(1)->GetOutputTextures();
		this->GetDrawTarget()->SetConfig(config);
	}
}

bool NormalRenderer::SetOutputSize(std::tuple<int, int> dimensions)
{
	this->m_rt_depth_only->SetOutputSize(dimensions);
	this->m_rt_draw->SetOutputSize(dimensions);

	Camera* camera = this->GetTarget()->GetCamera();
	std::tuple<int, int> ssr_quality_dimensions = std::tuple<int, int>(1, 1);
	if (camera != nullptr)
	{
		glm::ivec2 ssr_region_size = camera->GetSSRRegionDimensions();
		glm::ivec2 ssr_texture_size = glm::ivec2(glm::vec2(std::get<0>(dimensions), std::get<1>(dimensions)) / glm::vec2(ssr_region_size));
		ssr_quality_dimensions = std::tuple<int, int>(std::max(ssr_texture_size.x, 1), std::max(ssr_texture_size.y, 1));
	}

	this->m_rt_ssrquality->SetOutputSize(ssr_quality_dimensions);
	for (const std::unique_ptr<RenderTexture>& render_texture : this->m_rt_ssrquality_boxblur)
	{
		render_texture->SetOutputSize(ssr_quality_dimensions);
	}

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
			this->GetDepthOnlyTarget()->CopyFrom(src_renderer->GetDepthOnlyTarget());
			this->GetDrawTarget()->CopyFrom(src_renderer->GetDrawTarget());
			this->GetSSRQualityTarget()->CopyFrom(src_renderer->GetSSRQualityTarget());
			this->GetSSRBoxBlurTarget(1)->CopyFrom(src_renderer->GetSSRBoxBlurTarget(1));
			this->GetTarget()->CopyFrom(src_renderer->GetTarget());
		}
	}
}

std::unordered_set<RenderTextureReference> NormalRenderer::GetRenderTextureDependencies() const
{
	std::unordered_set<RenderTextureReference> references = this->GetDepthOnlyTarget()->GetRenderTextureDependencies();

	{
		std::unordered_set<RenderTextureReference> references_to_add = this->GetDrawTarget()->GetRenderTextureDependencies();
		references.insert(references_to_add.begin(), references_to_add.end());
	}

	{
		std::unordered_set<RenderTextureReference> references_to_add = this->GetTarget()->GetRenderTextureDependencies();
		references.insert(references_to_add.begin(), references_to_add.end());
	}

	{
		std::unordered_set<RenderTextureReference> references_to_add = this->GetSSRQualityTarget()->GetRenderTextureDependencies();
		references.insert(references_to_add.begin(), references_to_add.end());
	}

	for (const std::unique_ptr<RenderTexture>& render_texture : this->m_rt_ssrquality_boxblur)
	{
		std::unordered_set<RenderTextureReference> references_to_add = render_texture->GetRenderTextureDependencies();
		references.insert(references_to_add.begin(), references_to_add.end());
	}

	return references;
}

void NormalRenderer::Render(std::vector<Model*> models, bool continuous_draw)
{
	const RenderTargetConfig& target_config = this->GetTarget()->GetConfig();

	if (this->GetDepthOnlyTarget()->GetConfig().clear_fbo != this->GetTarget()->GetConfig().clear_fbo)
	{
		RenderTargetConfig config = this->GetDepthOnlyTarget()->GetConfig();
		config.clear_fbo = target_config.clear_fbo;
		this->GetDepthOnlyTarget()->SetConfig(config);
	}

	this->m_rt_depth_only->SetCamera(this->GetTarget()->GetCamera());
	this->m_rt_depth_only->SetOutputSize(this->GetTarget()->GetOutputSize());

	if (this->GetDrawTarget()->GetConfig().clear_fbo != this->GetTarget()->GetConfig().clear_fbo)
	{
		RenderTargetConfig config = this->GetDrawTarget()->GetConfig();
		config.clear_fbo = target_config.clear_fbo;
		this->GetDrawTarget()->SetConfig(config);
	}

	this->m_rt_draw->SetCamera(this->GetTarget()->GetCamera());
	this->m_rt_draw->SetOutputSize(this->GetTarget()->GetOutputSize());

	this->m_rt_depth_only->Render(models, continuous_draw);
	this->m_rt_depth_only->GetWriteTextures()->depth.value().CopyTo(this->m_rt_draw->GetWriteTextures()->depth.value());

	this->m_rt_draw->Render(models, continuous_draw);
	this->GetTarget()->Render(std::vector<Model*>(), continuous_draw);

	if (this->GetDrawTarget()->GetConfig().Data<RenderTargetConfig::Normal_Draw>().draw_reflections)
	{
		this->m_rt_ssrquality->Render(std::vector<Model*>(), continuous_draw);
		for (const std::unique_ptr<RenderTexture>& render_texture : this->m_rt_ssrquality_boxblur)
		{
			render_texture->Render(std::vector<Model*>(), continuous_draw);
		}
	}
}

RenderTexture* NormalRenderer::GetDepthOnlyTarget() const
{
	return this->m_rt_depth_only.get();
}

RenderTexture* NormalRenderer::GetDrawTarget() const
{
	return this->m_rt_draw.get();
}

RenderTexture* NormalRenderer::GetSSRQualityTarget() const
{
	return this->m_rt_ssrquality.get();
}

RenderTexture* NormalRenderer::GetSSRBoxBlurTarget(int index) const
{
	return this->m_rt_ssrquality_boxblur.at(index).get();
}
