#include "ReflectionController.h"

#include <stdexcept>

#include "../../Engine.h"

#include "../../scene/model/Reflection.h"
#include "../../scene/Cubemap.h"
#include "../../scene/Scene.h"

#include "../renderer/NormalRenderer.h"

#include "../TargetType.h"

#include "../rendertarget/texture/RenderTextureGroup.h"
#include "../rendertarget/texture/RenderTexture.h"

std::unique_ptr<Renderer> ReflectionController::GenerateRenderer(int layer)
{
	RenderTargetConfig config;
	config.SetMode(RenderTargetMode::Normal_PostProcess);
	config.clear_fbo = layer == 0;

	std::shared_ptr<RenderTextureGroup> textures = std::make_shared<RenderTextureGroup>(RenderTargetMode::Normal_PostProcess, TargetType::Texture_Cubemap);

	RenderTexture* render_texture = new RenderTexture(this->GetReference(), this->m_engine, config, textures);
	render_texture->SetOutputSize(this->m_cubemap->GetTextureDimensions());
	render_texture->SetCamera(this->m_camera.get());

	this->m_textures.push_back(std::move(std::unique_ptr<RenderTexture>(render_texture)));

	std::unique_ptr<NormalRenderer> renderer = std::make_unique<NormalRenderer>(this->m_engine, render_texture);
	return renderer;
}

bool ReflectionController::RepeatingConfigureRenderer(Renderer* renderer) const
{
	std::shared_ptr<Reflection> reflection = std::static_pointer_cast<Reflection>(this->m_cubemap);
	NormalRenderer* normal_renderer = dynamic_cast<NormalRenderer*>(renderer);

	bool updated = false;
	
	RenderTargetConfig config = normal_renderer->GetDrawTarget()->GetConfig();
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
		normal_renderer->GetDrawTarget()->SetConfig(config);
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

std::shared_ptr<Cubemap> ReflectionController::GetTargetCubemap() const
{
	std::optional<std::shared_ptr<Reflection>> reflection = this->m_engine->GetScene()->GetReflection(this->GetReference());
	if (reflection.has_value())
	{
		return reflection.value();
	}
	else
	{
		throw std::invalid_argument("Reference does not refer to an existing reflection");
	}
}