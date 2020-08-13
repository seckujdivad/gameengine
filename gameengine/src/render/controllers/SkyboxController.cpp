#include "SkyboxController.h"

SkyboxController::SkyboxController(Engine* engine, RenderTextureReference reference) : RenderController(engine, reference)
{
	this->m_texture = new RenderTexture(reference, engine, RenderMode::Normal, ENGINECANVAS_NUM_DATA_TEX, GL_TEXTURE_CUBE_MAP, false);
}

SkyboxController::~SkyboxController()
{
	delete this->m_texture;
}

void SkyboxController::Render()
{
	this->m_texture->Render();
}

RenderTextureGroup SkyboxController::GetRenderTexture()
{
	return this->m_texture->GetOutputTextures();
}

double SkyboxController::GetRenderGroup()
{
	return 0.0;
}

RenderControllerType SkyboxController::GetType()
{
	return RenderControllerType::Skybox;
}
