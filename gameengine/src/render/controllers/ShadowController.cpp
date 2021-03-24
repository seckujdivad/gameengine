#include "ShadowController.h"

#include "../rendertarget/texture/RenderTexture.h"
#include "../../scene/model/Reflection.h"
#include "../../scene/Cubemap.h"
#include "../renderer/WrapperRenderer.h"
#include "../TargetType.h"
#include "../rendertarget/texture/RenderTextureGroup.h"
#include "../../scene/texture/TextureFiltering.h"

std::unique_ptr<Renderer> ShadowController::GenerateRenderer(int layer)
{
	RenderTargetConfig config = RenderTargetConfig(RenderTargetMode::Shadow);
	if (layer != 0)
	{
		config.clear_fbo = false;
	}
	
	std::shared_ptr<RenderTextureGroup> textures = std::make_unique<RenderTextureGroup>(RenderTargetMode::Shadow, TargetType::Texture_Cubemap);

	textures->depth->SetTexParameter(GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
	textures->depth->SetTexParameter(GL_TEXTURE_COMPARE_FUNC, GL_LEQUAL);
	textures->depth->SetFiltering(TextureFiltering::Linear);

	std::shared_ptr<RenderTexture> render_texture = std::make_shared<RenderTexture>(this->GetReference(), this->m_engine, config, textures, false);
	this->m_textures.push_back(render_texture);
	render_texture->SetOutputSize(this->m_cubemap->GetTextureDimensions());
	render_texture->SetCamera(this->m_camera.get());

	std::unique_ptr<WrapperRenderer> renderer = std::make_unique<WrapperRenderer>(this->m_engine, render_texture.get());
	return renderer;
}

bool ShadowController::RepeatingConfigureRenderer(Renderer* renderer) const
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
