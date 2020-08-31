#include "ReflectionController.h"

#include "../RenderTexture.h"
#include "../../scene/Camera.h"
#include "../../scene/Cubemap.h"
#include "../../Engine.h"
#include "../../scene/Scene.h"

ReflectionController::ReflectionController(Engine* engine, RenderTextureReference reference) : RenderController(engine, reference)
{
	RenderTextureInfo info;
	info.colour = true;
	info.depth = true;
	info.num_data = GAMEENGINE_NUM_DATA_TEX;

	this->m_texture = new RenderTexture(reference, engine, RenderMode::Normal, info, GL_TEXTURE_CUBE_MAP, true);

	NormalRenderModeData data;
	data.previous_frame = this->m_texture->GetOutputTextures();
	this->m_texture->SetRenderMode(data);

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

	this->m_texture->SetOutputSize(cubemap->GetTextureDimensions());

	this->m_camera = new Camera();
	this->m_camera->SetPosition(cubemap->GetPosition());
	this->m_camera->SetRotation(0.0, 0.0, 0.0);
	this->m_camera->SetClips(cubemap->GetClips());
	this->m_camera->SetFOV(90.0);
	this->m_camera->SetViewportDimensions(cubemap->GetTextureDimensions());

	this->m_texture->SetCamera(this->m_camera);

	if (cubemap->GetDynamicRedrawFrames() > 0)
	{
		this->m_frame_counter = std::rand() % cubemap->GetDynamicRedrawFrames();
	}
	else
	{
		this->m_frame_counter = 0;
	}
}

ReflectionController::~ReflectionController()
{
	delete this->m_texture;
	delete this->m_camera;
}

void ReflectionController::Render()
{
	Cubemap* cubemap = std::get<0>(this->m_engine->GetScene()->GetCubemap(this->GetReference()));
	if (cubemap == nullptr)
	{
		throw std::runtime_error("Reflection no longer exists");
	}

	if (cubemap->GetDynamicRedrawFrames() != -1)
	{
		if (cubemap->GetDynamicRedrawFrames() <= this->m_frame_counter)
		{
			NormalRenderModeData data;
			data.previous_frame = this->m_texture->GetOutputTextures();
			this->m_texture->SetRenderMode(data);

			this->m_camera->SetPosition(cubemap->GetPosition());
			this->m_camera->SetClips(cubemap->GetClips());
			this->m_camera->SetViewportDimensions(cubemap->GetTextureDimensions());

			this->m_texture->SetOutputSize(cubemap->GetTextureDimensions());

			this->m_texture->Render();

			this->m_frame_counter = 0;
		}
		else
		{
			this->m_frame_counter++;
		}
	}
}

RenderTextureGroup ReflectionController::GetRenderTexture() const
{
	return this->m_texture->GetOutputTextures();
}

double ReflectionController::GetRenderGroup() const
{
	return 0.5; //reflections are dependent on shadows
}

RenderControllerType ReflectionController::GetType() const
{
	return RenderControllerType::Reflection;
}
