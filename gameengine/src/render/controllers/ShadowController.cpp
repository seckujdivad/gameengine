#include "ShadowController.h"

#include "../rendertarget/RenderTexture.h"
#include "../../scene/model/Reflection.h"
#include "../../scene/Cubemap.h"

RenderTexture* ShadowController::GenerateRenderTexture(int layer) const
{
	RenderTextureInfo info;
	info.colour = false;
	info.depth = true;
	info.num_data = 0;

	RenderTargetConfig config = { RenderTargetMode::Shadow, RenderTargetConfig::Shadow() };
	if (layer != 0)
	{
		config.clear_fbo = false;
	}

	RenderTexture* render_texture = new RenderTexture(this->GetReference(), this->m_engine, config, info, GL_TEXTURE_CUBE_MAP, false);
	render_texture->SetOutputSize(this->m_cubemap->GetTextureDimensions());
	render_texture->SetCamera(this->m_camera);

	return render_texture;
}

bool ShadowController::RepeatingConfigureRenderTexture(RenderTexture* render_texture) const
{
	return false;
}

ShadowController::ShadowController(Engine* engine, RenderTextureReference reference) : CubemapController(engine, reference)
{
	this->DerivedClassConstructedEvent();
}

RenderControllerType ShadowController::GetType() const
{
	return RenderControllerType::Shadow;
}

CubemapType ShadowController::GetCubemapType() const
{
	return CubemapType::Pointlight;
}
