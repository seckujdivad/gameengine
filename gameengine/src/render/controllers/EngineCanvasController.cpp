#include "EngineCanvasController.h"

#include "../../Engine.h"
#include "../EngineCanvas.h"
#include "../RenderTexture.h"

EngineCanvasController::EngineCanvasController(Engine* engine, RenderTextureReference reference, EngineCanvas* canvas, std::vector<CompositeLayer> composites)
    : RenderController(engine, reference),
    m_canvas(canvas)
{
    if (composites.size() == 0)
    {
        throw std::invalid_argument("You must provide at least one composite layer");
    }

    RenderableConfig postprocess_config = { RenderMode::Postprocess, RenderableConfig::PostProcess() };

    for (CompositeLayer composite : composites)
    {
        RenderTextureInfo info;
        info.colour = true;
        info.depth = true;

        RenderTexture* const render_texture = new RenderTexture(reference, engine, composite.config, info, GL_TEXTURE_2D, true);
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

    RenderTextureInfo postprocess_texture_info;
    postprocess_texture_info.colour = true;
    postprocess_texture_info.depth = false;
    postprocess_texture_info.num_data = 0;

    this->m_texture_final = new RenderTexture(reference, engine, postprocess_config, postprocess_texture_info, GL_TEXTURE_2D, false);

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
    if (this->m_canvas->GetOutputSize() != this->m_dimensions_prev)
    {
        for (RenderTexture* render_texture : this->m_textures)
        {
            render_texture->SetOutputSize(this->m_canvas->GetOutputSize());
        }

        this->m_texture_final->SetOutputSize(this->m_canvas->GetOutputSize());

        this->m_dimensions_prev = this->m_canvas->GetOutputSize();
    }

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

double EngineCanvasController::GetRenderGroup() const
{
    return 1.0; //last render
}

RenderControllerType EngineCanvasController::GetType() const
{
    return RenderControllerType::EngineCanvas;
}

EngineCanvas* EngineCanvasController::GetEngineCanvas() const
{
    return this->m_canvas;
}
