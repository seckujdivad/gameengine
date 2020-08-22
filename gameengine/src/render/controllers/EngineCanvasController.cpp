#include "EngineCanvasController.h"

EngineCanvasController::EngineCanvasController(Engine* engine, RenderTextureReference reference, EngineCanvas* canvas, RenderMode mode) : RenderController(engine, reference)
{
    this->m_canvas = canvas;

    RenderTextureInfo info;
    info.colour = true;
    info.depth = true;

    this->m_texture = new RenderTexture(reference, engine, mode, info, GL_TEXTURE_2D, true);
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

RenderTextureGroup EngineCanvasController::GetRenderTexture()
{
    return this->m_texture->GetOutputTextures();
}

double EngineCanvasController::GetRenderGroup()
{
    return 1.0; //last render
}

RenderControllerType EngineCanvasController::GetType()
{
    return RenderControllerType::EngineCanvas;
}

EngineCanvas* EngineCanvasController::GetEngineCanvas()
{
    return this->m_canvas;
}
