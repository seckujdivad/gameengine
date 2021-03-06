#include "ShadowController.h"

#include <stdexcept>

#include "../rendertarget/texture/RenderTexture.h"
#include "../../scene/Cubemap.h"
#include "../renderer/WrapperRenderer.h"
#include "../TargetType.h"
#include "../rendertarget/texture/RenderTextureGroup.h"
#include "../../scene/texture/TextureFiltering.h"
#include "../../scene/light/PointLight.h"
#include "../../Engine.h"
#include "../../scene/Scene.h"

std::unique_ptr<Renderer> ShadowController::GenerateRenderer(int layer)
{
	RenderTargetConfig config = RenderTargetConfig(RenderTargetMode::Shadow);
	config.clear_fbo = layer == 0;
	
	std::shared_ptr<RenderTextureGroup> textures = std::make_shared<RenderTextureGroup>(RenderTargetMode::Shadow, TargetType::Texture_Cubemap);

	textures->depth.value()->SetShadowCompareFunc(GL_LEQUAL);
	textures->depth.value()->SetFiltering(TextureFiltering::Linear);

	textures->depth.value()->SetLabel(textures->depth.value()->GetLabel().value() + " " + std::to_string(layer));

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

std::shared_ptr<Cubemap> ShadowController::GetTargetCubemap() const
{
	std::optional<std::shared_ptr<PointLight>> pointlight = this->m_engine->GetScene()->GetPointLight(this->GetReference());
	if (pointlight.has_value())
	{
		return pointlight.value();
	}
	else
	{
		throw std::invalid_argument("Reference does not refer to an existing point light");
	}
}