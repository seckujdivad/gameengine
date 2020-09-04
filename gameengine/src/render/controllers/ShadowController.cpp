#include "ShadowController.h"

#include "../RenderTexture.h"
#include "../../scene/model/Reflection.h"
#include "../../scene/Cubemap.h"

RenderTextureInfo ShadowController::GetRenderTextureInfo() const
{
	RenderTextureInfo info;
	info.colour = false;
	info.depth = true;
	info.num_data = 0;
	return info;
}

RenderMode ShadowController::GetRenderMode() const
{
	return RenderMode::Shadow;
}

void ShadowController::InitialConfigureRenderTexture(RenderTexture* render_texture) const
{
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
