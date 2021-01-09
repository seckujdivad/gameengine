#include "CubemapController.h"

#include <tuple>
#include <stdexcept>

#include "../../Engine.h"
#include "../rendertarget/RenderTexture.h"
#include "../../scene/Scene.h"
#include "../../scene/Camera.h"

void CubemapController::DerivedClassConstructedEvent()
{
	//get cubemap and check type
	std::tuple<Cubemap*, CubemapType> cubemap_data = this->m_engine->GetScene()->GetCubemap(this->GetReference());
	if (std::get<0>(cubemap_data) == nullptr)
	{
		throw std::runtime_error("Invalid reference given to controller - nullptr returned when retrieving cubemap");
	}
	if (std::get<1>(cubemap_data) != this->GetCubemapType())
	{
		throw std::runtime_error("Invalid reference given to controller - cubemap returned does not match type " + static_cast<int>(this->GetCubemapType()));
	}

	this->m_cubemap = std::get<0>(cubemap_data);

	if (this->m_cubemap->GetDynamicRedrawFrames() > 0)
	{
		this->m_frame_counter = std::rand() % this->m_cubemap->GetDynamicRedrawFrames(); //add a random offset so that cubemaps with the same refresh rate aren't all drawn on the same frame
	}
	else
	{
		this->m_frame_counter = 0;
	}

	//initialise camera
	this->m_camera = new Camera();
	this->m_camera->SetPosition(this->m_cubemap->GetPosition());
	this->m_camera->SetRotation(0.0, 0.0, 0.0);
	this->m_camera->SetClips(this->m_cubemap->GetClips());
	this->m_camera->SetFOV(90.0);
	this->m_camera->SetViewportDimensions(this->m_cubemap->GetTextureDimensions());

	//initialise 2 render textures for the cumulative texture
	this->m_render_textures.reserve(2);
	for (int i = 0; i < 2; i++) //0 = static render, 1 = dynamic render
	{
		RenderTexture* render_texture = this->GenerateRenderTexture(i);

		render_texture->SetRenderFunction(
			[render_texture, cubemap = this->m_cubemap, engine = this->m_engine, layer = i]
		(std::vector<Model*> models_input)
		{
			std::vector<Model*> model_ptrs;
			if (layer == 0)
			{
				model_ptrs = engine->GetScene()->GetModels(cubemap->GetStaticModels());
			}
			else if (layer == 1)
			{
				model_ptrs = engine->GetScene()->GetModels(cubemap->GetDynamicModels());
			}
			else
			{
				throw std::invalid_argument("Invalid layer: " + std::to_string(layer));
			}

			render_texture->RenderScene(model_ptrs);
		});

		this->m_render_textures.push_back(render_texture);
	}

	//reinitialise cumulative texture with new render textures
	this->m_cumulative_texture = CumulativeTexture(this->m_render_textures);
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
	for (RenderTexture* render_texture : this->m_render_textures)
	{
		delete render_texture;
	}

	delete this->m_camera;
}

void CubemapController::Render()
{
	int redraw_level = -1; //the texture that the redraw starts at - -1 means no redraw at all

	if (!this->m_render_textures.at(0)->FramebufferContainsRenderOutput()) //textures have never been drawn to, redraw static textures as well as dynamic
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
			
			for (RenderTexture* render_texture : this->m_render_textures)
			{
				render_texture->SetOutputSize(this->m_cubemap->GetTextureDimensions());

				if (this->RepeatingConfigureRenderTexture(render_texture)) //returning true means data has changed
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
		this->m_cumulative_texture.Render(redraw_level);
	}
}

RenderTextureGroup CubemapController::GetRenderTexture() const
{
	return this->m_cumulative_texture.GetOutput()->GetOutputTextures();
}

std::unordered_set<RenderTextureReference> CubemapController::GetRenderTextureDependencies() const
{
	std::unordered_set<RenderTextureReference> result;
	for (RenderTexture* render_texture : this->m_render_textures)
	{
		for (RenderTextureReference reference : render_texture->GetRenderTextureDependencies())
		{
			result.insert(reference);
		}
	}

	return result;
}
