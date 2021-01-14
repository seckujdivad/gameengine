#include "ShadowController.h"

#include "../rendertarget/RenderTexture.h"
#include "../../scene/model/Reflection.h"
#include "../../scene/Cubemap.h"
#include "../renderjob/WrapperRenderJobFactory.h"

std::unique_ptr<RenderJobFactory> ShadowController::GenerateFactory(int layer)
{
	RenderTextureInfo info;
	info.colour = false;
	info.depth = true;
	info.num_data = 0;

	RenderTargetConfig config = { RenderTargetMode::Shadow, RenderTargetConfig::Shadow() };
	if (layer != 0)
	{
		config.clear_fbo = false;
	}

	this->m_textures.push_back(std::move(std::make_unique<RenderTexture>(this->GetReference(), this->m_engine, config, info, GL_TEXTURE_CUBE_MAP, false)));
	RenderTexture* render_texture = (*this->m_textures.rbegin()).get();
	render_texture->SetOutputSize(this->m_cubemap->GetTextureDimensions());
	render_texture->SetCamera(this->m_camera.get());

	std::unique_ptr<WrapperRenderJobFactory> factory = std::make_unique<WrapperRenderJobFactory>(this->m_engine, render_texture);
	return factory;
}

bool ShadowController::RepeatingConfigureFactory(RenderJobFactory* factory) const
{
	return false;
}

ShadowController::ShadowController(Engine* engine, RenderTextureReference reference) : CubemapController(engine, reference)
{
	this->DerivedClassConstructedEvent();
}

RenderControllerType ShadowController::GetType() const
{
	return RenderControllerType::Shadow;
}

CubemapType ShadowController::GetCubemapType() const
{
	return CubemapType::Pointlight;
}
