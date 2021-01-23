#include "ReflectionController.h"

#include <stdexcept>

#include "../rendertarget/RenderTexture.h"
#include "../../scene/model/Reflection.h"
#include "../../scene/Cubemap.h"
#include "../renderjob/NormalRenderJobFactory.h"
#include "../NormalModeConstants.h"

std::unique_ptr<RenderJobFactory> ReflectionController::GenerateFactory(int layer)
{
	RenderTextureInfo info;
	info.colour = true;
	info.depth = true;
	info.num_data = GAMEENGINE_NUM_DATA_TEX;

	RenderTargetConfig config = { RenderTargetMode::Normal_LastPass, RenderTargetConfig::Normal_LastPass() };
	if (layer != 0)
	{
		config.clear_fbo = false;
	}

	this->m_textures.push_back(std::move(std::make_unique<RenderTexture>(this->GetReference(), this->m_engine, config, info, GL_TEXTURE_CUBE_MAP, false, layer != 1)));
	RenderTexture* render_texture = (*this->m_textures.rbegin()).get();
	render_texture->SetOutputSize(this->m_cubemap->GetTextureDimensions());
	render_texture->SetCamera(this->m_camera.get());

	std::unique_ptr<NormalRenderJobFactory> factory = std::make_unique<NormalRenderJobFactory>(this->m_engine, render_texture);
	return factory;
}

bool ReflectionController::RepeatingConfigureFactory(RenderJobFactory* factory) const
{
	Reflection* reflection = static_cast<Reflection*>(this->m_cubemap);

	NormalRenderJobFactory* normal_factory = dynamic_cast<NormalRenderJobFactory*>(factory);

	bool updated = normal_factory->SetDrawReflections(reflection->GetDrawReflections());
	updated = normal_factory->SetDrawReflections(reflection->GetDrawReflections()) ? true : updated;

	return updated;
}

ReflectionController::ReflectionController(Engine* engine, RenderTextureReference reference) : CubemapController(engine, reference)
{
	this->DerivedClassConstructedEvent();
}

void ReflectionController::PostRender()
{
	this->m_factories.at(1)->GetTarget()->SwapBuffers();
}

RenderControllerType ReflectionController::GetType() const
{
	return RenderControllerType::Reflection;
}

CubemapType ReflectionController::GetCubemapType() const
{
	return CubemapType::Reflection;
}
