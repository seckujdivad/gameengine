#pragma once

#include "RenderController.h"

class SkyboxController : public RenderController
{
private:
	RenderTexture* m_texture;

public:
	SkyboxController(Engine* engine, RenderTextureReference reference);
	SkyboxController(const SkyboxController&) = delete;
	SkyboxController& operator=(const SkyboxController&) = delete;
	~SkyboxController();

	void Render() override;
	RenderTextureGroup GetRenderTexture() override;
	double GetRenderGroup() override;
	RenderControllerType GetType() override;
};