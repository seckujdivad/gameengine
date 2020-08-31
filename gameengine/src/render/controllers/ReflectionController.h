#pragma once

class RenderTexture;
class Camera;

#include "RenderController.h"

class ReflectionController : public RenderController
{
private:
	RenderTexture* m_texture;

	Camera* m_camera;

	int m_frame_counter;

public:
	ReflectionController(Engine* engine, RenderTextureReference reference);
	ReflectionController(const ReflectionController&) = delete;
	ReflectionController& operator=(const ReflectionController&) = delete;
	ReflectionController(ReflectionController&&) = delete;
	ReflectionController& operator=(ReflectionController&&) = delete;
	~ReflectionController();

	void Render() override;
	RenderTextureGroup GetRenderTexture() const override;
	double GetRenderGroup() const override;
	RenderControllerType GetType() const override;
};