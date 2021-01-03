#include "RenderController.h"

RenderController::RenderController(Engine* engine, RenderTextureReference reference) : Referenceable<RenderTextureReference>(reference)
{
	this->m_engine = engine;
}

RenderController::~RenderController()
{
}

void RenderController::PostRender()
{
}

bool RenderController::IsEssentialDraw() const
{
	return false;
}
