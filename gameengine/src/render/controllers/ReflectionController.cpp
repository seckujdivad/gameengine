#include "ReflectionController.h"

ReflectionController::ReflectionController(Engine* engine, RenderTextureReference reference) : RenderController(engine, reference)
{
	this->m_texture = new RenderTexture(reference, engine, RenderMode::Normal, 0, GL_TEXTURE_2D, false);

	std::tuple<Cubemap*, CubemapType> cubemap_data = this->m_engine->GetScene()->GetCubemap(reference);
	Cubemap* cubemap = std::get<0>(cubemap_data);
	if (cubemap == nullptr)
	{
		throw std::runtime_error("Invalid reference given to controller - nullptr returned when retrieving cubemap");
	}
	if (std::get<1>(cubemap_data) != CubemapType::Reflection)
	{
		throw std::runtime_error("Invalid reference given to controller - cubemap returned is not a reflection");
	}

	this->m_camera = new Camera();
	this->m_camera->SetPosition(cubemap->GetPosition());
	this->m_camera->SetRotation(0.0, 0.0, 0.0);
	this->m_camera->SetClips(cubemap->GetClips());
	this->m_camera->SetFOV(90.0);
	this->m_camera->SetViewportDimensions(cubemap->GetTextureDimensions());
}

ReflectionController::~ReflectionController()
{
	delete this->m_texture;
	delete this->m_camera;
}

void ReflectionController::Render()
{
	Cubemap* cubemap = std::get<0>(this->m_engine->GetScene()->GetCubemap(this->GetReference()));
	this->m_camera->SetPosition(cubemap->GetPosition());
	this->m_camera->SetClips(cubemap->GetClips());
	this->m_camera->SetViewportDimensions(cubemap->GetTextureDimensions());

	this->m_texture->Render();
}

RenderTextureGroup ReflectionController::GetRenderTexture()
{
	return this->m_texture->GetOutputTextures();
}

double ReflectionController::GetRenderGroup()
{
	return 0.5; //reflections are dependent on shadows
}

RenderControllerType ReflectionController::GetType()
{
	return RenderControllerType::Reflection;
}
