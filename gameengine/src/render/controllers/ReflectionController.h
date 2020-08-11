#pragma once

#include "RenderController.h"
#include "../../scene/Camera.h"
#include "../../scene/Cubemap.h"

class ReflectionController : public RenderController
{
private:
	RenderTexture* m_texture;

	Camera* m_camera;

public:
	ReflectionController(Engine* engine, RenderTextureReference reference);
	ReflectionController(const ReflectionController&) = delete;
	ReflectionController& operator=(const ReflectionController&) = delete;
	~ReflectionController();

	void Render() override;
	RenderTextureGroup GetRenderTexture() override;
	double GetRenderGroup() override;
	RenderControllerType GetType() override;
};