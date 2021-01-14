#pragma once

#include <vector>
#include <memory>
#include <optional>

#include "RenderController.h"
#include "../CumulativeTexture.h"
#include "../../scene/Referenceable.h"
#include "../../scene/Cubemap.h"
#include "../rendertarget/RenderTargetMode.h"

class Camera;
class Model;
class RenderJobFactory;
class RenderTexture;

class CubemapController : public RenderController
{
protected:
	std::vector<std::unique_ptr<RenderJobFactory>> m_factories;
	std::vector<std::unique_ptr<RenderTexture>> m_textures;
	std::optional<CumulativeTexture> m_cumulative_texture;

	int m_frame_counter;

	Cubemap* m_cubemap;
	std::unique_ptr<Camera> m_camera;

	void DerivedClassConstructedEvent(); //this MUST be called by the derived constructor

	virtual std::unique_ptr<RenderJobFactory> GenerateFactory(int layer) = 0;
	virtual bool RepeatingConfigureFactory(RenderJobFactory* render_texture) const = 0;

public:
	CubemapController(Engine* engine, RenderTextureReference reference);
	virtual ~CubemapController();

	void Render() override;
	RenderTextureGroup GetRenderTexture() const override;
	virtual RenderControllerType GetType() const = 0;

	virtual CubemapType GetCubemapType() const = 0;

	std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const override;
};