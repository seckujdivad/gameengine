#include "EngineCanvasController.h"

EngineCanvasController::EngineCanvasController(Engine* engine, RenderTextureReference reference, EngineCanvas* canvas, RenderMode mode) : RenderController(engine, reference)
{
    this->m_canvas = canvas;

    this->m_texture = new RenderTexture(reference, engine, mode, 1, GL_TEXTURE_2D, true);
}

EngineCanvasController::~EngineCanvasController()
{
    delete this->m_texture;
}

void EngineCanvasController::Render()
{
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
