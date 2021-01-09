#include "SkyboxController.h"

#include "../rendertarget/RenderTexture.h"
#include "../../scene/model/Reflection.h"
#include "../../scene/Cubemap.h"

RenderTexture* SkyboxController::GenerateRenderTexture(int layer) const
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
	render_texture->SetCamera(this->m_camera);
	render_texture->SetNormalModePreviousFrameToSelf();

	return render_texture;
}

bool SkyboxController::RepeatingConfigureRenderTexture(RenderTexture* render_texture) const
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
