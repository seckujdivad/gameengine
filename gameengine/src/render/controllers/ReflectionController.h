#pragma once

#include <vector>

#include "RenderController.h"
#include "../CumulativeTexture.h"

class RenderTexture;
class Camera;
class Reflection;
class Model;

class ReflectionController : public RenderController
{
private:
	std::vector<RenderTexture*> m_render_textures;

	CumulativeTexture m_cumulative_texture;

	Camera* m_camera;
	Reflection* m_reflection;

	int m_frame_counter;

	std::vector<Model*> m_models_static;
	std::vector<Model*> m_models_dynamic;

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