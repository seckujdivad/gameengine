#include "CubemapController.h"

#include <tuple>
#include <stdexcept>

#include "../../Engine.h"
#include "../rendertarget/texture/RenderTexture.h"
#include "../../scene/Scene.h"
#include "../../scene/Camera.h"
#include "../renderer/Renderer.h"

void CubemapController::DerivedClassConstructedEvent()
{
	//get cubemap and check type
	this->m_cubemap = this->GetTargetCubemap();

	if (this->m_cubemap->GetDynamicRedrawFrames() > 0)
	{
		this->m_frame_counter = std::rand() % this->m_cubemap->GetDynamicRedrawFrames(); //add a random offset so that cubemaps with the same refresh rate aren't all drawn on the same frame
	}
	else
	{
		this->m_frame_counter = 0;
	}

	//initialise camera
	this->m_camera = std::make_unique<Camera>();
	this->m_camera->SetPosition(this->m_cubemap->GetPosition());
	this->m_camera->SetRotation(glm::dvec3(0.0, 0.0, 0.0));
	this->m_camera->SetClips(this->m_cubemap->GetClips());
	this->m_camera->SetFOV(90.0);
	this->m_camera->SetViewportDimensions(this->m_cubemap->GetTextureDimensions());

	//initialise 2 render textures for the cumulative texture
	this->m_renderers.reserve(2);
	for (int i = 0; i < 2; i++) //0 = static render, 1 = dynamic render
	{
		this->m_renderers.push_back(this->GenerateRenderer(i));
	}

	//reinitialise cumulative texture with new render textures
	{
		std::vector<Renderer*> renderers;
		renderers.reserve(this->m_renderers.size());
		for (const auto& renderer : this->m_renderers)
		{
			renderers.push_back(renderer.get());
		}
		this->m_cumulative_texture = CumulativeTexture(renderers);

		this->m_cumulative_texture->SetFetchModelsFunction([&, engine = this->m_engine, cubemap = this->m_cubemap](int layer)
		{
			std::vector<std::shared_ptr<Model>> models;
			if (layer == 0)
			{
				models = engine->GetScene()->GetModels(cubemap->GetStaticModels());
			}
			else if (layer == 1)
			{
				models = engine->GetScene()->GetModels(cubemap->GetDynamicModels());
			}
			else
			{
				throw std::invalid_argument("Invalid layer: " + std::to_string(layer));
			}

			std::vector<Model*> raw_ptrs;
			raw_ptrs.reserve(models.size());
			for (const std::shared_ptr<Model>& model : models)
			{
				raw_ptrs.push_back(model.get());
			}

			return raw_ptrs;
		});
	}
}

//some members aren't initialised - this is fine if the derived class calls DerivedClassConstructedEvent (which it must)
#pragma warning(push)
#pragma warning(disable: 26495) 
CubemapController::CubemapController(Engine* engine, RenderTextureReference reference) : RenderController(engine, reference)
#pragma warning(pop)
{
}

CubemapController::~CubemapController()
{
}

void CubemapController::Render(std::clock_t draw_time, bool continuous_draw)
{
	int redraw_level = -1; //the texture that the redraw starts at - -1 means no redraw at all

	if (!this->m_renderers.at(0)->GetTarget()->FramebufferContainsRenderOutput()) //textures have never been drawn to, redraw static textures as well as dynamic
	{
		redraw_level = 0;
	}
	else if (this->m_cubemap->GetDynamicRedrawFrames() != -1) //check if dynamic redraws ever take place
	{
		if (this->m_cubemap->GetDynamicRedrawFrames() <= this->m_frame_counter)
		{
			bool static_redraw_required = false;

			if (this->m_camera->GetPosition() != this->m_cubemap->GetPosition())
			{
				static_redraw_required = true;
				this->m_camera->SetPosition(this->m_cubemap->GetPosition());
			}
			
			if (this->m_camera->GetClips() != this->m_cubemap->GetClips())
			{
				static_redraw_required = true;
				this->m_camera->SetClips(this->m_cubemap->GetClips());
			}
			
			if (this->m_camera->GetViewportDimensions() != this->m_cubemap->GetTextureDimensions())
			{
				static_redraw_required = true;
				this->m_camera->SetViewportDimensions(this->m_cubemap->GetTextureDimensions());
			}
			
			for (const auto& renderer : this->m_renderers)
			{
				renderer->SetOutputSize(this->m_cubemap->GetTextureDimensions());

				if (this->RepeatingConfigureRenderer(renderer.get())) //returning true means data has changed
				{
					static_redraw_required = true;
				}
			}

			if (static_redraw_required)
			{
				redraw_level = 0;
			}
			else
			{
				redraw_level = 1;
			}

			this->m_frame_counter = 0;
		}
		else
		{
			this->m_frame_counter++;
		}
	}

	if (redraw_level != -1)
	{
		this->m_cumulative_texture->Render(draw_time, redraw_level);
	}
}

std::shared_ptr<RenderTextureGroup> CubemapController::GetRenderTexture() const
{
	return dynamic_cast<RenderTexture*>(this->m_cumulative_texture->GetOutput())->GetOutputTextures();
}

std::unordered_set<RenderTextureReference> CubemapController::GetRenderTextureDependencies() const
{
	std::unordered_set<RenderTextureReference> result;
	for (const auto& renderer : this->m_renderers)
	{
		for (RenderTextureReference reference : renderer->GetRenderTextureDependencies())
		{
			result.insert(reference);
		}
	}

	return result;
}
