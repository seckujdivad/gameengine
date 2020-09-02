#include "ReflectionController.h"

#include "../RenderTexture.h"
#include "../../scene/Camera.h"
#include "../../scene/Cubemap.h"
#include "../../Engine.h"
#include "../../scene/Scene.h"

ReflectionController::ReflectionController(Engine* engine, RenderTextureReference reference) : RenderController(engine, reference)
{
	std::tuple<Cubemap*, CubemapType> cubemap_data = this->m_engine->GetScene()->GetCubemap(reference);
	if (std::get<0>(cubemap_data) == nullptr)
	{
		throw std::runtime_error("Invalid reference given to controller - nullptr returned when retrieving cubemap");
	}
	if (std::get<1>(cubemap_data) != CubemapType::Reflection)
	{
		throw std::runtime_error("Invalid reference given to controller - cubemap returned is not a reflection");
	}

	this->m_reflection = static_cast<Reflection*>(std::get<0>(cubemap_data));

	this->m_camera = new Camera();
	this->m_camera->SetPosition(this->m_reflection->GetPosition());
	this->m_camera->SetRotation(0.0, 0.0, 0.0);
	this->m_camera->SetClips(this->m_reflection->GetClips());
	this->m_camera->SetFOV(90.0);
	this->m_camera->SetViewportDimensions(this->m_reflection->GetTextureDimensions());

	this->m_models_static = this->m_engine->GetScene()->GetModels(this->m_reflection->GetStaticModels());
	this->m_models_dynamic = this->m_engine->GetScene()->GetModels(this->m_reflection->GetDynamicModels());

	{
		RenderTextureInfo info;
		info.colour = true;
		info.depth = true;
		info.num_data = GAMEENGINE_NUM_DATA_TEX;

		NormalRenderModeData data;

		for (int i = 0; i < 2; i++)
		{
			RenderTexture* render_texture = new RenderTexture(reference, engine, RenderMode::Normal, info, GL_TEXTURE_CUBE_MAP, false);
			render_texture->SetOutputSize(this->m_reflection->GetTextureDimensions());
			render_texture->SetCamera(this->m_camera);

			std::vector<Model*> models;
			if (i == 0)
			{
				models = this->m_models_static;
			}
			else if (i == 1)
			{
				models = this->m_models_dynamic;
			}

			render_texture->SetRenderFunction([render_texture, models](std::vector<Model*> models_input)
			{
				render_texture->RenderScene(models);
			});

			data.previous_frame = render_texture->GetOutputTextures();
			render_texture->SetRenderMode(data);

			this->m_render_textures.push_back(render_texture);
		}
	}

	this->m_render_textures.at(1)->GetConfig().clear_fbo = false;

	this->m_cumulative_texture = CumulativeTexture(this->m_render_textures);

	if (this->m_reflection->GetDynamicRedrawFrames() > 0)
	{
		this->m_frame_counter = std::rand() % this->m_reflection->GetDynamicRedrawFrames();
	}
	else
	{
		this->m_frame_counter = 0;
	}
}

ReflectionController::~ReflectionController()
{
	for (RenderTexture* render_texture : this->m_render_textures)
	{
		delete render_texture;
	}

	delete this->m_camera;
}

void ReflectionController::Render()
{
	if (!this->m_render_textures.at(0)->FramebufferContainsRenderOutput())
	{
		this->m_cumulative_texture.Render();
	}

	if (this->m_reflection->GetDynamicRedrawFrames() != -1)
	{
		if (this->m_reflection->GetDynamicRedrawFrames() <= this->m_frame_counter)
		{
			this->m_camera->SetPosition(this->m_reflection->GetPosition());
			this->m_camera->SetClips(this->m_reflection->GetClips());
			this->m_camera->SetViewportDimensions(this->m_reflection->GetTextureDimensions());

			bool static_redraw_required = false;
			for (RenderTexture* render_texture : this->m_render_textures)
			{
				static_redraw_required = render_texture->SetOutputSize(this->m_reflection->GetTextureDimensions());
			}

			if (static_redraw_required)
			{
				this->m_cumulative_texture.Render(0);
			}
			else
			{
				this->m_cumulative_texture.Render(1);
			}

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
	return this->m_cumulative_texture.GetOutput()->GetOutputTextures();
}

double ReflectionController::GetRenderGroup() const
{
	return 0.5; //reflections are dependent on shadows
}

RenderControllerType ReflectionController::GetType() const
{
	return RenderControllerType::Reflection;
}
