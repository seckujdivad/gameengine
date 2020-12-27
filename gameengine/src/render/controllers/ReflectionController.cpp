#include "ReflectionController.h"

#include <stdexcept>

#include "../RenderTexture.h"
#include "../../scene/model/Reflection.h"
#include "../../scene/Cubemap.h"

RenderTexture* ReflectionController::GenerateRenderTexture(int layer) const
{
	RenderTextureInfo info;
	info.colour = true;
	info.depth = true;
	info.num_data = GAMEENGINE_NUM_DATA_TEX;

	RenderableConfig config = { RenderMode::Normal, RenderableConfig::Normal() };
	if (layer != 0)
	{
		config.clear_fbo = false;
	}

	std::get<RenderableConfig::Normal>(config.mode_data).draw_shadows = false;

	RenderTexture* render_texture = new RenderTexture(this->GetReference(), this->m_engine, config, info, GL_TEXTURE_CUBE_MAP, true, layer != 1);
	render_texture->SetOutputSize(this->m_cubemap->GetTextureDimensions());
	render_texture->SetCamera(this->m_camera);
	render_texture->SetNormalModePreviousFrameToSelf();

	return render_texture;
}

bool ReflectionController::RepeatingConfigureRenderTexture(RenderTexture* render_texture) const
{
	Reflection* reflection = static_cast<Reflection*>(this->m_cubemap);

	if (render_texture->GetRenderMode() == RenderMode::Normal)
	{
		if (std::get<RenderableConfig::Normal>(render_texture->GetConfig().mode_data).draw_shadows == reflection->GetDrawShadows())
		{
			return false;
		}
		else
		{
			RenderableConfig config = render_texture->GetConfig();
			std::get<RenderableConfig::Normal>(config.mode_data).draw_shadows = reflection->GetDrawShadows();
			render_texture->SetConfig(config);

			return true;
		}
	}
	else
	{
		throw std::runtime_error("Render mode must be \"Normal\", not " + std::to_string(static_cast<int>(render_texture->GetRenderMode())));
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

RenderControllerType ReflectionController::GetType() const
{
	return RenderControllerType::Reflection;
}

CubemapType ReflectionController::GetCubemapType() const
{
	return CubemapType::Reflection;
}
