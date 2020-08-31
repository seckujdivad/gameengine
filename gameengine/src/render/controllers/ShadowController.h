#pragma once

class Camera;
class RenderTexture;

#include "RenderController.h"

class ShadowController : public RenderController
{
private:
	RenderTexture* m_texture;

	Camera* m_camera;

public:
	ShadowController(Engine* engine, RenderTextureReference reference);
	ShadowController(const ShadowController&) = delete;
	ShadowController& operator=(const ShadowController&) = delete;
	~ShadowController();

	void Render() override;
	RenderTextureGroup GetRenderTexture() const override;
	double GetRenderGroup() const override;
	RenderControllerType GetType() const override;
};