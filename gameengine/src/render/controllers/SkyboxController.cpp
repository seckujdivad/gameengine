#include "SkyboxController.h"

#include "../rendertarget/RenderTexture.h"
#include "../../scene/model/Reflection.h"
#include "../../scene/Cubemap.h"
#include "../renderer/NormalRenderer.h"

std::unique_ptr<Renderer> SkyboxController::GenerateRenderer(int layer)
{
	RenderTextureInfo info;
	info.colour = true;
	info.depth = true;
	info.num_data = GAMEENGINE_NUM_DATA_TEX;

	RenderTargetConfig config = { RenderTargetMode::Normal, RenderTargetConfig::Normal() };
	if (layer != 0)
	{
		config.clear_fbo = false;
	}

	std::get<RenderTargetConfig::Normal>(config.mode_data).draw_shadows = false;

	RenderTexture* render_texture = new RenderTexture(this->GetReference(), this->m_engine, config, info, GL_TEXTURE_CUBE_MAP, true);
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
