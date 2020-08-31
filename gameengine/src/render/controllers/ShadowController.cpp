#include "ShadowController.h"

#include "../RenderTexture.h"
#include "../../scene/Camera.h"
#include "../../scene/Cubemap.h"
#include "../../Engine.h"
#include "../../scene/Scene.h"

ShadowController::ShadowController(Engine* engine, RenderTextureReference reference) : RenderController(engine, reference)
{
	RenderTextureInfo info;
	info.colour = false;
	info.depth = true;
	info.depth_filtering = GL_LINEAR;
	info.num_data = 0;

	this->m_texture = new RenderTexture(reference, engine, RenderMode::Shadow, info, GL_TEXTURE_CUBE_MAP, false);

	std::tuple<Cubemap*, CubemapType> cubemap_data = this->m_engine->GetScene()->GetCubemap(reference);
	Cubemap* cubemap = std::get<0>(cubemap_data);
	if (cubemap == nullptr)
	{
		throw std::runtime_error("Invalid reference given to controller - nullptr returned when retrieving cubemap");
	}
	if (std::get<1>(cubemap_data) != CubemapType::Pointlight)
	{
		throw std::runtime_error("Invalid reference given to controller - cubemap returned is not a point light");
	}

	this->m_texture->SetOutputSize(cubemap->GetTextureDimensions());

	this->m_camera = new Camera();
	this->m_camera->SetPosition(cubemap->GetPosition());
	this->m_camera->SetRotation(0.0, 0.0, 0.0);
	this->m_camera->SetClips(cubemap->GetClips());
	this->m_camera->SetFOV(90.0);
	this->m_camera->SetViewportDimensions(cubemap->GetTextureDimensions());

	this->m_texture->SetCamera(this->m_camera);
}

ShadowController::~ShadowController()
{
	delete this->m_texture;
	delete this->m_camera;
}

void ShadowController::Render()
{
	Cubemap* cubemap = std::get<0>(this->m_engine->GetScene()->GetCubemap(this->GetReference()));
	if (cubemap == nullptr)
	{
		throw std::runtime_error("Point light no longer exists");
	}

	this->m_camera->SetPosition(cubemap->GetPosition());
	this->m_camera->SetClips(cubemap->GetClips());
	this->m_camera->SetViewportDimensions(cubemap->GetTextureDimensions());

	this->m_texture->SetOutputSize(cubemap->GetTextureDimensions());

	this->m_texture->Render();
}

RenderTextureGroup ShadowController::GetRenderTexture() const
{
	return this->m_texture->GetOutputTextures();
}

double ShadowController::GetRenderGroup() const
{
	return 0.0; //shadows have no dependencies, draw first
}

RenderControllerType ShadowController::GetType() const
{
	return RenderControllerType::Shadow;
}
