#pragma once

#include "RenderController.h"

class RenderTexture;

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
	RenderTextureGroup GetRenderTexture() const override;
	double GetRenderGroup() const override;
	RenderControllerType GetType() const override;
};