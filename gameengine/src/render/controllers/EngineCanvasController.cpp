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

    {
        RenderTextureInfo info;
        info.colour = true;
        info.depth = false;
        info.num_data = 0;

        this->m_texture_final = new RenderTexture(reference, engine, RenderMode::Postprocess, info, GL_TEXTURE_2D, false);
    }

    {
        PostProcessRenderModeData postprocess_data;

        for (const CompositeLayer& composite : composites)
        {
            RenderTextureInfo info;
            info.colour = true;
            info.depth = true;

            RenderTexture* const render_texture = new RenderTexture(reference, engine, composite.mode, info, GL_TEXTURE_2D, true);
            this->m_textures.push_back(render_texture);

            if (composite.mode == RenderMode::Normal)
            {
                NormalRenderModeData data;
                data.previous_frame = render_texture->GetOutputTextures();
                render_texture->SetRenderMode(data);
            }
            else if (composite.mode == RenderMode::Wireframe)
            {
                WireframeRenderModeData data;
                render_texture->SetRenderMode(data);
            }
            else if (composite.mode == RenderMode::Textured)
            {
                TexturedRenderModeData data;
                render_texture->SetRenderMode(data);
            }
            else
            {
                throw std::invalid_argument("Unknown render mode " + std::to_string(static_cast<int>(composite.mode)));
            }

            PostProcessRenderModeData::CompositeLayer layer;
            layer.id = render_texture->GetOutputTextures().colour;

            postprocess_data.layers.push_back(layer);
        }

        this->m_texture_final->SetRenderMode(postprocess_data);
    }

    {
        PostProcessRenderModeData postprocess_data;
        PostProcessRenderModeData::CompositeLayer layer;

        layer.id = this->m_texture_final->GetOutputTextures().colour;

        postprocess_data.layers.push_back(layer);
        this->m_canvas->SetRenderMode(postprocess_data);
    }
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
