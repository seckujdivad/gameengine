#pragma once

#include <vector>
#include <memory>
#include <optional>

#include "RenderController.h"
#include "../renderable/CumulativeTexture.h"
#include "../../scene/Referenceable.h"
#include "../../scene/Cubemap.h"
#include "../rendertarget/target/RenderTargetMode.h"

class Camera;
class Model;
class Renderer;
class RenderTexture;

class CubemapController : public RenderController
{
protected:
	std::vector<std::unique_ptr<Renderer>> m_renderers;
	std::vector<std::unique_ptr<RenderTexture>> m_textures;
	std::optional<CumulativeTexture> m_cumulative_texture;

	int m_frame_counter;

	Cubemap* m_cubemap;
	std::unique_ptr<Camera> m_camera;

	void DerivedClassConstructedEvent(); //this MUST be called by the derived constructor

	virtual std::unique_ptr<Renderer> GenerateRenderer(int layer) = 0;
	virtual bool RepeatingConfigureRenderer(Renderer* renderer) const = 0;

public:
	CubemapController(Engine* engine, RenderTextureReference reference);
	virtual ~CubemapController();

	void Render() override;
	std::shared_ptr<RenderTextureGroup> GetRenderTexture() const override;
	virtual RenderControllerType GetType() const = 0;

	virtual CubemapType GetCubemapType() const = 0;

	std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const override;
};