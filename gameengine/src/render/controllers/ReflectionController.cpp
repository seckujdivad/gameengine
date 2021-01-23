#include "ReflectionController.h"

#include <stdexcept>

#include "../rendertarget/RenderTexture.h"
#include "../../scene/model/Reflection.h"
#include "../../scene/Cubemap.h"
#include "../renderer/NormalRenderer.h"

std::unique_ptr<Renderer> ReflectionController::GenerateRenderer(int layer)
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

	this->m_textures.push_back(std::move(std::make_unique<RenderTexture>(this->GetReference(), this->m_engine, config, info, GL_TEXTURE_CUBE_MAP, true, layer != 1)));
	RenderTexture* render_texture = (*this->m_textures.rbegin()).get();
	render_texture->SetOutputSize(this->m_cubemap->GetTextureDimensions());
	render_texture->SetCamera(this->m_camera.get());
	render_texture->SetNormalModePreviousFrameToSelf();

	std::unique_ptr<NormalRenderer> renderer = std::make_unique<NormalRenderer>(this->m_engine, render_texture);
	return renderer;
}

bool ReflectionController::RepeatingConfigureRenderer(Renderer* renderer) const
{
	Reflection* reflection = static_cast<Reflection*>(this->m_cubemap);

	bool updated = false;
	
	RenderTargetConfig config = renderer->GetTarget()->GetConfig();
	if (reflection->GetDrawShadows() != std::get<RenderTargetConfig::Normal>(config.mode_data).draw_shadows)
	{
		std::get<RenderTargetConfig::Normal>(config.mode_data).draw_shadows = reflection->GetDrawShadows();
		updated = true;
	}

	if (reflection->GetDrawReflections() != std::get<RenderTargetConfig::Normal>(config.mode_data).draw_reflections)
	{
		std::get<RenderTargetConfig::Normal>(config.mode_data).draw_reflections = reflection->GetDrawReflections();
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

void ReflectionController::PostRender()
{
	this->m_renderers.at(1)->GetTarget()->SwapBuffers();
}

RenderControllerType ReflectionController::GetType() const
{
	return RenderControllerType::Reflection;
}

CubemapType ReflectionController::GetCubemapType() const
{
	return CubemapType::Reflection;
}
