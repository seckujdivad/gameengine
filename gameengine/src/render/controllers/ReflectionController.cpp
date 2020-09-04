#include "ReflectionController.h"

#include "../RenderTexture.h"
#include "../../scene/model/Reflection.h"
#include "../../scene/Cubemap.h"

RenderTextureInfo ReflectionController::GetRenderTextureInfo() const
{
	RenderTextureInfo info;
	info.colour = true;
	info.depth = true;
	info.num_data = GAMEENGINE_NUM_DATA_TEX;
	return info;
}

RenderMode ReflectionController::GetRenderMode() const
{
	return RenderMode::Normal;
}

void ReflectionController::InitialConfigureRenderTexture(RenderTexture* render_texture) const
{
	NormalRenderModeData render_data;
	render_data.draw_shadows = false;
	render_data.previous_frame = render_texture->GetOutputTextures();
	render_texture->SetRenderMode(render_data);
}

bool ReflectionController::RepeatingConfigureRenderTexture(RenderTexture* render_texture) const
{
	Reflection* reflection = static_cast<Reflection*>(this->m_cubemap);

	if (render_texture->GetNormalRenderModeData().draw_shadows == reflection->GetDrawShadows())
	{
		return false;
	}
	else
	{
		render_texture->GetNormalRenderModeData().draw_shadows = reflection->GetDrawShadows();
		return true;
	}
}

ReflectionController::ReflectionController(Engine* engine, RenderTextureReference reference) : CubemapController(engine, reference)
{
	this->DerivedClassConstructedEvent();
}

double ReflectionController::GetRenderGroup() const
{
	return 0.5;
}

RenderControllerType ReflectionController::GetType() const
{
	return RenderControllerType::Reflection;
}

CubemapType ReflectionController::GetCubemapType() const
{
	return CubemapType::Reflection;
}
