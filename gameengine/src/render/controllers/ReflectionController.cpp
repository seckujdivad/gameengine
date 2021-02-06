#include "ReflectionController.h"

#include <stdexcept>

#include "../rendertarget/RenderTexture.h"
#include "../../scene/model/Reflection.h"
#include "../../scene/Cubemap.h"
#include "../renderer/NormalRenderer.h"
#include "../TargetType.h"

std::unique_ptr<Renderer> ReflectionController::GenerateRenderer(int layer)
{
	RenderTargetConfig config;
	config.SetMode(RenderTargetMode::Normal_Draw);
	config.clear_fbo = layer == 0;

	std::unique_ptr<RenderTextureGroup> textures = std::make_unique<RenderTextureGroup>(RenderTargetMode::Normal_Draw, TargetType::Texture_Cubemap);

	RenderTexture* render_texture = new RenderTexture(this->GetReference(), this->m_engine, config, textures.get(), true, true);
	render_texture->SetOutputSize(this->m_cubemap->GetTextureDimensions());
	render_texture->SetCamera(this->m_camera.get());
	render_texture->SetNormalModePreviousFrameToSelf();

	this->m_textures.push_back(std::move(std::unique_ptr<RenderTexture>(render_texture)));

	std::unique_ptr<NormalRenderer> renderer = std::make_unique<NormalRenderer>(this->m_engine, render_texture);
	return renderer;
}

bool ReflectionController::RepeatingConfigureRenderer(Renderer* renderer) const
{
	Reflection* reflection = static_cast<Reflection*>(this->m_cubemap);

	bool updated = false;
	
	RenderTargetConfig config = renderer->GetTarget()->GetConfig();
	if (reflection->GetDrawShadows() != std::get<RenderTargetConfig::Normal_Draw>(config.mode_data).draw_shadows)
	{
		std::get<RenderTargetConfig::Normal_Draw>(config.mode_data).draw_shadows = reflection->GetDrawShadows();
		updated = true;
	}

	if (reflection->GetDrawReflections() != std::get<RenderTargetConfig::Normal_Draw>(config.mode_data).draw_reflections)
	{
		std::get<RenderTargetConfig::Normal_Draw>(config.mode_data).draw_reflections = reflection->GetDrawReflections();
		updated = true;
	}

	if (updated)
	{
		renderer->GetTarget()->SetConfig(config);
	}

	return updated;
}

ReflectionController::ReflectionController(Engine* engine, RenderTextureReference reference) : CubemapController(engine, reference)
{
	this->DerivedClassConstructedEvent();
}

RenderControllerType ReflectionController::GetType() const
{
	return RenderControllerType::Reflection;
}

CubemapType ReflectionController::GetCubemapType() const
{
	return CubemapType::Reflection;
}
