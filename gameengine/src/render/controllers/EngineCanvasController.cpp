#include "EngineCanvasController.h"

#include <stdexcept>

#include "../../Engine.h"
#include "../../scene/Scene.h"
#include "../rendertarget/canvas/EngineCanvas.h"
#include "../rendertarget/texture/RenderTexture.h"
#include "../rendertarget/texture/RenderTextureGroup.h"
#include "../renderer/WrapperRenderer.h"
#include "../renderer/NormalRenderer.h"
#include "../TargetType.h"

RenderTargetConfig EngineCanvasController::RemakeTextures(std::vector<EngineCanvasController::CompositeLayer> composite_layers)
{
	if (composite_layers.size() == 0)
	{
		throw std::invalid_argument("You must provide at least one composite layer");
	}

	this->m_renderers.clear();
	this->m_textures.clear();

	//make new textures
	RenderTargetConfig postprocess_config;
	postprocess_config.SetMode(RenderTargetMode::PostProcess);

	for (const CompositeLayer& composite : composite_layers)
	{
		RenderTargetConfig cfg;
		RenderTargetMode render_target_mode;
		if (composite.mode == RenderMode::Normal)
		{
			render_target_mode = RenderTargetMode::Normal_PostProcess;
		}
		else if (composite.mode == RenderMode::Wireframe)
		{
			render_target_mode = RenderTargetMode::Wireframe;
		}
		else if (composite.mode == RenderMode::Textured)
		{
			render_target_mode = RenderTargetMode::Textured;
		}
		else
		{
			throw std::invalid_argument("Unsupported render mode " + std::to_string(static_cast<int>(composite.mode)));
		}

		cfg.SetMode(render_target_mode);

		std::unique_ptr<RenderTextureGroup> textures = std::make_unique<RenderTextureGroup>(render_target_mode, TargetType::Texture_2D);

		this->m_textures.push_back(std::make_unique<RenderTexture>(this->GetReference(), this->m_engine, cfg, textures.get()));
		RenderTexture* render_texture = (*this->m_textures.rbegin()).get();

		std::unique_ptr<Renderer> renderer;
		if (composite.mode == RenderMode::Normal)
		{
			renderer = std::make_unique<NormalRenderer>(this->m_engine, render_texture);
		}
		else
		{
			renderer = std::make_unique<WrapperRenderer>(this->m_engine, render_texture);
		}

		RenderTargetConfig::PostProcess::CompositeLayer layer;
		layer.texture = &render_texture->GetOutputTextures()->colour.at(0);
		std::get<RenderTargetConfig::PostProcess>(postprocess_config.mode_data).layers.push_back(layer);

		this->m_renderers.push_back(std::move(renderer));
	}

	return postprocess_config;
}

EngineCanvasController::EngineCanvasController(Engine* engine, RenderTextureReference reference, EngineCanvas* canvas, std::vector<CompositeLayer> composites)
	: RenderController(engine, reference),
	m_canvas(canvas)
{
	std::unique_ptr<RenderTextureGroup> textures = std::make_unique<RenderTextureGroup>(RenderTargetMode::PostProcess, TargetType::Texture_2D);
	this->m_texture_final = std::make_unique<RenderTexture>(reference, engine, this->RemakeTextures(composites), textures.get(), false);

	RenderTargetConfig canvas_config;
	canvas_config.SetMode(RenderTargetMode::PostProcess);

	RenderTargetConfig::PostProcess::CompositeLayer passthrough_layer;
	passthrough_layer.texture = &this->m_texture_final->GetOutputTextures()->colour.at(0);
	std::get<RenderTargetConfig::PostProcess>(canvas_config.mode_data).layers.push_back(passthrough_layer);

	this->m_canvas->SetConfig(canvas_config);
}

void EngineCanvasController::Render()
{
	//resize textures to make sure that all textures in the chain are the same size/drawing at the same resolution
	std::tuple new_output_size = this->m_canvas->GetOutputSize();
	for (std::unique_ptr<Renderer>& renderer : this->m_renderers)
	{
		renderer->SetOutputSize(new_output_size);
	}
	this->m_texture_final->SetOutputSize(new_output_size);

	//redraw all textures
	std::vector<Model*> models = this->m_engine->GetScene()->GetModels();
	for (std::unique_ptr<Renderer>& factory : this->m_renderers)
	{
		factory->SetCamera(this->m_canvas->GetControlledCamera());
		factory->Render(models);
	}

	this->m_texture_final->Render({});
	this->m_canvas->Render({}, true);
}

std::shared_ptr<RenderTextureGroup> EngineCanvasController::GetRenderTexture() const
{
	return this->m_texture_final->GetOutputTextures();
}

RenderControllerType EngineCanvasController::GetType() const
{
	return RenderControllerType::EngineCanvas;
}

EngineCanvas* EngineCanvasController::GetEngineCanvas() const
{
	return this->m_canvas;
}

void EngineCanvasController::SetRenderLayers(std::vector<EngineCanvasController::CompositeLayer> composite_layers)
{
	this->m_texture_final->SetConfig(this->RemakeTextures(composite_layers));
}

void EngineCanvasController::SetRenderLayers(std::vector<RenderMode> modes)
{
	std::vector<EngineCanvasController::CompositeLayer> composite_layers;
	composite_layers.reserve(modes.size());
	for (RenderMode mode : modes)
	{
		EngineCanvasController::CompositeLayer layer;
		layer.mode = mode;
		composite_layers.push_back(layer);
	}

	return this->SetRenderLayers(composite_layers);
}

void EngineCanvasController::SetRenderLayers(RenderMode mode)
{
	return this->SetRenderLayers(std::vector({ mode }));
}

std::unordered_set<RenderTextureReference> EngineCanvasController::GetRenderTextureDependencies() const
{
	std::unordered_set<RenderTextureReference> result;
	for (const std::unique_ptr<Renderer>& factory : this->m_renderers)
	{
		for (RenderTextureReference reference : factory->GetRenderTextureDependencies())
		{
			result.insert(reference);
		}
	}

	for (RenderTextureReference reference : this->m_texture_final->GetRenderTextureDependencies())
	{
		result.insert(reference);
	}

	for (RenderTextureReference reference : this->m_canvas->GetRenderTextureDependencies())
	{
		result.insert(reference);
	}

	return result;
}

bool EngineCanvasController::IsEssentialDraw() const
{
	return true;
}
