#include "RenderController.h"

#include "../../Engine.h"

RenderController::RenderController(Engine* engine, RenderTextureReference reference) : Referenceable<RenderTextureReference>(reference)
{
	this->m_engine = engine;
}

RenderController::~RenderController()
{
}
