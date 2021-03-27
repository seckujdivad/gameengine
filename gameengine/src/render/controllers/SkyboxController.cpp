#include "SkyboxController.h"

#include <stdexcept>

#include "../rendertarget/texture/RenderTexture.h"
#include "../../scene/Cubemap.h"
#include "../renderer/NormalRenderer.h"
#include "../TargetType.h"
#include "../rendertarget/texture/RenderTextureGroup.h"
#include "../../scene/Scene.h"
#include "../../Engine.h"
#include "../../scene/Skybox.h"

std::unique_ptr<Renderer> SkyboxController::GenerateRenderer(int layer)
{
	RenderTargetConfig config;
	config.SetMode(RenderTargetMode::Normal_PostProcess);
	if (layer != 0)
	{
		config.clear_fbo = false;
	}

	std::get<RenderTargetConfig::Normal_Draw>(config.mode_data).draw_shadows = false;

	std::shared_ptr<RenderTextureGroup> textures = std::make_shared<RenderTextureGroup>(RenderTargetMode::Normal_PostProcess, TargetType::Texture_Cubemap);

	RenderTexture* render_texture = new RenderTexture(this->GetReference(), this->m_engine, config, textures, true);
	render_texture->SetOutputSize(this->m_cubemap->GetTextureDimensions());
	render_texture->SetCamera(this->m_camera.get());

	std::unique_ptr<NormalRenderer> renderer = std::make_unique<NormalRenderer>(this->m_engine, render_texture);
	return renderer;
}

bool SkyboxController::RepeatingConfigureRenderer(Renderer* renderer) const
{
	return false;
}

SkyboxController::SkyboxController(Engine* engine, RenderTextureReference reference) : CubemapController(engine, reference)
{
	this->DerivedClassConstructedEvent();
}

RenderControllerType SkyboxController::GetType() const
{
	return RenderControllerType::Skybox;
}

std::shared_ptr<Cubemap> SkyboxController::GetTargetCubemap() const
{
	std::optional<std::shared_ptr<Skybox>> skybox = this->m_engine->GetScene()->GetSkybox(this->GetReference());
	if (skybox.has_value())
	{
		return skybox.value();
	}
	else
	{
		throw std::invalid_argument("Reference does not refer to an existing skybox");
	}
}