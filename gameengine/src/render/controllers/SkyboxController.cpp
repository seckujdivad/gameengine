#include "SkyboxController.h"

SkyboxController::SkyboxController(Engine* engine, RenderTextureReference reference) : RenderController(engine, reference)
{
	RenderTextureInfo info;
	info.colour = true;
	info.depth = true;

	this->m_texture = new RenderTexture(reference, engine, RenderMode::Normal, info, GL_TEXTURE_CUBE_MAP, false);
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
