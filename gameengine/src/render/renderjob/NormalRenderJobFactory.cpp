#include "NormalRenderJobFactory.h"

#include <stdexcept>

#include "NormalRenderJob.h"
#include "../rendertarget/RenderTexture.h"

NormalRenderJobFactory::NormalRenderJobFactory(Engine* engine, RenderTarget* target) : RenderJobFactory(engine, target), m_camera(target->GetCamera())
{
	if (target->GetRenderMode() != RenderTargetMode::Normal_LastPass)
	{
		throw std::invalid_argument("Target must have mode Normal_LastPass");
	}

	this->m_default_initialiser = std::make_unique<NormalRenderJobInitialiser>();

	{
		RenderTargetConfig config = { RenderTargetMode::Normal_FirstPass, RenderTargetConfig::Normal_FirstPass() };

		RenderTextureInfo info;
		info.num_data = GAMEENGINE_NORMAL_PASSTHROUGH_TEX - 1;
		info.colour_filtering = GL_NEAREST;

		this->m_rendertexture_first_pass = std::make_unique<RenderTexture>(-1, this->GetEngine(), config, info, target->GetTargetType(), false, true);
		this->m_rendertexture_first_pass->SetCamera(this->GetCamera());
	}

	{
		RenderTargetConfig config = { RenderTargetMode::Normal_PointLight, RenderTargetConfig::Normal_PointLight() };

		std::get<RenderTargetConfig::Normal_PointLight>(config.mode_data).first_pass = this->m_rendertexture_first_pass->GetOutputTextures();

		RenderTextureInfo info;
		info.num_data = 0;
		info.colour_filtering = GL_NEAREST;

		this->m_rendertexture_pointlight = std::make_unique<RenderTexture>(-1, this->GetEngine(), config, info, target->GetTargetType(), false, true);
		this->m_rendertexture_pointlight->SetCamera(this->GetCamera());
	}

	{
		RenderTargetConfig config = this->GetTarget()->GetConfig();
		std::get<RenderTargetConfig::Normal_LastPass>(config.mode_data).first_pass = this->m_rendertexture_first_pass->GetOutputTextures();
		std::get<RenderTargetConfig::Normal_LastPass>(config.mode_data).pointlight_pass = this->m_rendertexture_pointlight->GetOutputTextures();
		std::get<RenderTargetConfig::Normal_LastPass>(config.mode_data).camera = this->GetCamera();
		this->GetTarget()->SetConfig(config);
	}
}

bool NormalRenderJobFactory::SetOutputSize(std::tuple<int, int> dimensions)
{
	return this->GetTarget()->SetOutputSize(dimensions);
}

std::shared_ptr<RenderJob> NormalRenderJobFactory::GenerateJob(RenderJobInitialiser* initialiser)
{
	NormalRenderJobInitialiser* init;
	if (initialiser == nullptr)
	{
		init = this->m_default_initialiser.get();
	}
	else
	{
		init = dynamic_cast<NormalRenderJobInitialiser*>(initialiser);
	}

	if (init == nullptr)
	{
		throw std::invalid_argument("Initialiser must be castable to NormalRenderJobInitialiser as this is a NormalRenderJobFactory");
	}
	else
	{
		std::shared_ptr<NormalRenderJob> job = std::make_shared<NormalRenderJob>(this);
		return job;
	}
}

void NormalRenderJobFactory::CopyFrom(const RenderJobFactory* src) const
{
	if (src != this)
	{
		this->GetTarget()->CopyFrom(src->GetTarget());
	}
}

std::unordered_set<RenderTextureReference> NormalRenderJobFactory::GetRenderTextureDependencies() const
{
	return this->GetTarget()->GetRenderTextureDependencies();
}

NormalRenderJobInitialiser& NormalRenderJobFactory::GetDefaultInitialiser()
{
	return *this->m_default_initialiser;
}

void NormalRenderJobFactory::Render(std::vector<Model*> models, bool continuous_draw)
{
	if (this->GetCamera() != this->m_camera)
	{
		this->m_rendertexture_first_pass->SetCamera(this->GetCamera());
		this->m_rendertexture_pointlight->SetCamera(this->GetCamera());

		RenderTargetConfig config = this->GetTarget()->GetConfig();
		std::get<RenderTargetConfig::Normal_LastPass>(config.mode_data).camera = this->GetCamera();
		this->GetTarget()->SetConfig(config);

		this->m_camera = this->GetCamera();
	}

	
	this->m_rendertexture_first_pass->SetOutputSize(this->GetTarget()->GetOutputSize());
	this->m_rendertexture_pointlight->SetOutputSize(this->GetTarget()->GetOutputSize());

	this->m_rendertexture_first_pass->Render(models, continuous_draw);
	this->m_rendertexture_pointlight->Render({ }, continuous_draw);
	this->GetTarget()->Render({ }, continuous_draw);
}
