#include "EngineCanvasController.h"

#include "../../Engine.h"
#include "../../scene/Scene.h"
#include "../rendertarget/EngineCanvas.h"
#include "../rendertarget/RenderTexture.h"
#include "../renderer/WrapperRenderer.h"
#include "../renderer/NormalRenderer.h"

RenderTargetConfig EngineCanvasController::RemakeTextures(std::vector<EngineCanvasController::CompositeLayer> composite_layers)
{
	if (composite_layers.size() == 0)
	{
		throw std::invalid_argument("You must provide at least one composite layer");
	}

	this->m_renderers.clear();
	this->m_textures.clear();

	//make new textures
	RenderTargetConfig postprocess_config = { RenderTargetMode::Postprocess, RenderTargetConfig::PostProcess() };
	for (CompositeLayer composite : composite_layers)
	{
		RenderTextureInfo info;
		info.colour = true;
		info.depth = true;

		RenderTargetConfig cfg;
		if (composite.mode == RenderMode::Normal)
		{
			cfg.SetMode(RenderTargetMode::Normal_Draw);
		}
		else if (composite.mode == RenderMode::Wireframe)
		{
			cfg.SetMode(RenderTargetMode::Wireframe);
		}
		else if (composite.mode == RenderMode::Textured)
		{
			cfg.SetMode(RenderTargetMode::Textured);
		}
		else
		{
			throw std::invalid_argument("Unsupported render mode " + std::to_string(static_cast<int>(composite.mode)));
		}

		this->m_textures.push_back(std::make_unique<RenderTexture>(this->GetReference(), this->m_engine, cfg, info, GL_TEXTURE_2D, true));
		RenderTexture* render_texture = (*this->m_textures.rbegin()).get();

		std::unique_ptr<Renderer> renderer;
		if (composite.mode == RenderMode::Normal)
		{
			render_texture->SetNormalModePreviousFrameToSelf();
			renderer = std::make_unique<NormalRenderer>(this->m_engine, render_texture);
		}
		else
		{
			renderer = std::make_unique<WrapperRenderer>(this->m_engine, render_texture);
		}

		RenderTargetConfig::PostProcess::CompositeLayer layer;
		layer.id = render_texture->GetOutputTextures().colour;
		std::get<RenderTargetConfig::PostProcess>(postprocess_config.mode_data).layers.push_back(layer);

		this->m_renderers.push_back(std::move(renderer));
	}

	return postprocess_config;
}

EngineCanvasController::EngineCanvasController(Engine* engine, RenderTextureReference reference, EngineCanvas* canvas, std::vector<CompositeLayer> composites)
	: RenderController(engine, reference),
	m_canvas(canvas)
{
	RenderTextureInfo postprocess_texture_info;
	postprocess_texture_info.colour = true;
	postprocess_texture_info.depth = false;
	postprocess_texture_info.num_data = 0;

	this->m_texture_final = std::make_unique<RenderTexture>(reference, engine, this->RemakeTextures(composites), postprocess_texture_info, GL_TEXTURE_2D, false);

	RenderTargetConfig canvas_config = { RenderTargetMode::Postprocess, RenderTargetConfig::PostProcess() };

	RenderTargetConfig::PostProcess::CompositeLayer passthrough_layer;
	passthrough_layer.id = this->m_texture_final->GetOutputTextures().colour;
	std::get<RenderTargetConfig::PostProcess>(canvas_config.mode_data).layers.push_back(passthrough_layer);

	this->m_canvas->SetConfig(canvas_config);
}

void EngineCanvasController::Render()
{
	//resize textures to make sure that all textures in the chain are the same size/drawing at the same resolution
	std::tuple new_output_size = this->m_canvas->GetOutputSize();
	for (std::unique_ptr<Renderer>& factory : this->m_renderers)
	{
		factory->SetOutputSize(new_output_size);
	}
	this->m_texture_final->SetOutputSize(new_output_size);

	//redraw all textures
	std::vector<Model*> models = this->m_engine->GetScene()->GetModels();
	for (std::unique_ptr<Renderer>& factory : this->m_renderers)
	{
		factory->SetCamera(this->m_canvas->GetControlledCamera());
		factory->Render(models);
	}

	this->m_texture_final->Render(models);
	this->m_canvas->Render(models, true);
}

RenderTextureGroup EngineCanvasController::GetRenderTexture() const
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
