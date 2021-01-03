#include "EngineCanvasController.h"

#include "../EngineCanvas.h"
#include "../RenderTexture.h"

RenderableConfig EngineCanvasController::RemakeTextures(std::vector<EngineCanvasController::CompositeLayer> composite_layers)
{
	if (composite_layers.size() == 0)
	{
		throw std::invalid_argument("You must provide at least one composite layer");
	}

	//deallocate old textures
	for (RenderTexture* render_texture : this->m_textures)
	{
		delete render_texture;
	}
	this->m_textures.clear();

	//make new textures
	RenderableConfig postprocess_config = { RenderMode::Postprocess, RenderableConfig::PostProcess() };
	for (CompositeLayer composite : composite_layers)
	{
		RenderTextureInfo info;
		info.colour = true;
		info.depth = true;

		RenderTexture* const render_texture = new RenderTexture(this->GetReference(), this->m_engine, composite.config, info, GL_TEXTURE_2D, true);
		this->m_textures.push_back(render_texture);

		if (composite.config.mode == RenderMode::Normal)
		{
			std::get<RenderableConfig::Normal>(composite.config.mode_data).previous_frame = render_texture->GetOutputTextures();
			render_texture->SetConfig(composite.config);
		}

		RenderableConfig::PostProcess::CompositeLayer layer;
		layer.id = render_texture->GetOutputTextures().colour;
		std::get<RenderableConfig::PostProcess>(postprocess_config.mode_data).layers.push_back(layer);
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

	this->m_texture_final = new RenderTexture(reference, engine, this->RemakeTextures(composites), postprocess_texture_info, GL_TEXTURE_2D, false);

	RenderableConfig canvas_config = { RenderMode::Postprocess, RenderableConfig::PostProcess() };

	RenderableConfig::PostProcess::CompositeLayer passthrough_layer;
	passthrough_layer.id = this->m_texture_final->GetOutputTextures().colour;
	std::get<RenderableConfig::PostProcess>(canvas_config.mode_data).layers.push_back(passthrough_layer);

	this->m_canvas->SetConfig(canvas_config);
}

EngineCanvasController::~EngineCanvasController()
{
	for (RenderTexture* render_texture : this->m_textures)
	{
		delete render_texture;
	}

	delete this->m_texture_final;
}

void EngineCanvasController::Render()
{
	//resize textures to make sure that all textures in the chain are the same size/drawing at the same resolution
	std::tuple new_output_size = this->m_canvas->GetOutputSize();
	for (RenderTexture* render_texture : this->m_textures)
	{
		render_texture->SetOutputSize(new_output_size);
	}
	this->m_texture_final->SetOutputSize(new_output_size);

	//redraw all textures
	for (RenderTexture* render_texture : this->m_textures)
	{
		render_texture->SetCamera(this->m_canvas->GetControlledCamera());
		render_texture->Render();
	}

	this->m_texture_final->Render();
	this->m_canvas->Render();
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

void EngineCanvasController::SetRenderLayers(std::vector<RenderableConfig> configs)
{
	std::vector<EngineCanvasController::CompositeLayer> composite_layers;
	composite_layers.reserve(configs.size());
	for (const RenderableConfig& config : configs)
	{
		EngineCanvasController::CompositeLayer layer;
		layer.config = config;
		composite_layers.push_back(layer);
	}

	return this->SetRenderLayers(composite_layers);
}

void EngineCanvasController::SetRenderLayers(RenderableConfig config)
{
	return this->SetRenderLayers(std::vector({ config }));
}

std::unordered_set<RenderTextureReference> EngineCanvasController::GetRenderTextureDependencies() const
{
	std::unordered_set<RenderTextureReference> result;
	for (RenderTexture* render_texture : this->m_textures)
	{
		for (RenderTextureReference reference : render_texture->GetRenderTextureDependencies())
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
