#include "EngineCanvasController.h"

#include "../../Engine.h"
#include "../EngineCanvas.h"
#include "../RenderTexture.h"

EngineCanvasController::EngineCanvasController(Engine* engine, RenderTextureReference reference, EngineCanvas* canvas, RenderMode mode)
    : RenderController(engine, reference),
    m_canvas(canvas)
{
    RenderTextureInfo info;
    info.colour = true;
    info.depth = true;

    this->m_texture = new RenderTexture(reference, engine, mode, info, GL_TEXTURE_2D, true);
    if (mode == RenderMode::Normal)
    {
        NormalRenderModeData data;
        data.previous_frame = this->m_texture->GetOutputTextures();
        this->m_texture->SetRenderMode(data);
    }
    else if (mode == RenderMode::Wireframe)
    {
        WireframeRenderModeData data;
        this->m_texture->SetRenderMode(data);
    }
    else if (mode == RenderMode::Textured)
    {
        TexturedRenderModeData data;
        this->m_texture->SetRenderMode(data);
    }
    else
    {
        throw std::invalid_argument("Unknown render mode " + std::to_string(static_cast<int>(mode)));
    }

    {
        PostProcessRenderModeData data;
        data.textures.push_back(this->m_texture->GetOutputTextures().colour);
        this->m_canvas->SetRenderMode(data);
    }
}

EngineCanvasController::~EngineCanvasController()
{
    delete this->m_texture;
}

void EngineCanvasController::Render()
{
    if (this->m_canvas->GetOutputSize() != this->m_dimensions_prev)
    {
        this->m_texture->SetOutputSize(this->m_canvas->GetOutputSize());
        this->m_dimensions_prev = this->m_canvas->GetOutputSize();
    }

    this->m_texture->SetCamera(this->m_canvas->GetControlledCamera());
    this->m_texture->Render();
    this->m_canvas->Render();
}

RenderTextureGroup EngineCanvasController::GetRenderTexture() const
{
    return this->m_texture->GetOutputTextures();
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
