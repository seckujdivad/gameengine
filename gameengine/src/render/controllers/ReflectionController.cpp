#include "ReflectionController.h"

#include "../RenderTexture.h"
#include "../../scene/model/Reflection.h"
#include "../../scene/Cubemap.h"

RenderMode ReflectionController::GetRenderMode() const
{
	return RenderMode::Normal;
}

RenderTexture* ReflectionController::GenerateRenderTexture(int layer) const
{
	RenderTextureInfo info;
	info.colour = true;
	info.depth = true;
	info.num_data = GAMEENGINE_NUM_DATA_TEX;

	RenderTexture* render_texture = new RenderTexture(this->GetReference(), this->m_engine, this->GetRenderMode(), info, GL_TEXTURE_CUBE_MAP, true, layer != 1);
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

void ReflectionController::PostRender()
{
	this->m_render_textures.at(1)->SwapBuffers();
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
