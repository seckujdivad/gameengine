#include "SkyboxController.h"

#include "../RenderTexture.h"
#include "../../scene/model/Reflection.h"
#include "../../scene/Cubemap.h"

RenderMode SkyboxController::GetRenderMode() const
{
	return RenderMode::Normal;
}

RenderTexture* SkyboxController::GenerateRenderTexture(int layer) const
{
	RenderTextureInfo info;
	info.colour = true;
	info.depth = true;
	info.num_data = GAMEENGINE_NUM_DATA_TEX;

	RenderTexture* render_texture = new RenderTexture(this->GetReference(), this->m_engine, this->GetRenderMode(), info, GL_TEXTURE_CUBE_MAP, true);
	render_texture->SetOutputSize(this->m_cubemap->GetTextureDimensions());
	render_texture->SetCamera(this->m_camera);

	if (layer != 0)
	{
		render_texture->GetConfig().clear_fbo = false;
	}

	NormalRenderModeData render_data;
	render_data.draw_shadows = false;
	render_data.previous_frame = render_texture->GetOutputTextures();
	render_texture->SetRenderMode(render_data);

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

double SkyboxController::GetRenderGroup() const
{
	return 0.75;
}

RenderControllerType SkyboxController::GetType() const
{
	return RenderControllerType::Skybox;
}

CubemapType SkyboxController::GetCubemapType() const
{
	return CubemapType::Skybox;
}
