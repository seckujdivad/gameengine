#include "ShadowController.h"

#include "../RenderTexture.h"
#include "../../scene/model/Reflection.h"
#include "../../scene/Cubemap.h"

RenderMode ShadowController::GetRenderMode() const
{
	return RenderMode::Shadow;
}

RenderTexture* ShadowController::GenerateRenderTexture(int layer) const
{
	RenderTextureInfo info;
	info.colour = false;
	info.depth = true;
	info.num_data = 0;

	RenderTexture* render_texture = new RenderTexture(this->GetReference(), this->m_engine, this->GetRenderMode(), info, GL_TEXTURE_CUBE_MAP, false);
	render_texture->SetOutputSize(this->m_cubemap->GetTextureDimensions());
	render_texture->SetCamera(this->m_camera);

	if (layer != 0)
	{
		render_texture->GetConfig().clear_fbo = false;
	}

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

double ShadowController::GetRenderGroup() const
{
	return 0.0;
}

RenderControllerType ShadowController::GetType() const
{
	return RenderControllerType::Shadow;
}

CubemapType ShadowController::GetCubemapType() const
{
	return CubemapType::Pointlight;
}
