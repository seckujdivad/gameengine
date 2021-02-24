#include "SkyboxController.h"

#include "../rendertarget/texture/RenderTexture.h"
#include "../../scene/model/Reflection.h"
#include "../../scene/Cubemap.h"
#include "../renderer/NormalRenderer.h"
#include "../TargetType.h"
#include "../rendertarget/texture/RenderTextureGroup.h"

std::unique_ptr<Renderer> SkyboxController::GenerateRenderer(int layer)
{
	RenderTargetConfig config = { RenderTargetMode::Normal_Draw, RenderTargetConfig::Normal_Draw() };
	if (layer != 0)
	{
		config.clear_fbo = false;
	}

	std::get<RenderTargetConfig::Normal_Draw>(config.mode_data).draw_shadows = false;

	std::unique_ptr<RenderTextureGroup> textures = std::make_unique<RenderTextureGroup>(RenderTargetMode::Normal_Draw, TargetType::Texture_Cubemap);

	RenderTexture* render_texture = new RenderTexture(this->GetReference(), this->m_engine, config, textures.get(), true);
	render_texture->SetOutputSize(this->m_cubemap->GetTextureDimensions());
	render_texture->SetCamera(this->m_camera.get());
	render_texture->SetNormalModePreviousFrameToSelf();

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

CubemapType SkyboxController::GetCubemapType() const
{
	return CubemapType::Skybox;
}
