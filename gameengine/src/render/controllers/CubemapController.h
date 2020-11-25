#pragma once

#include <vector>

#include "RenderController.h"
#include "../CumulativeTexture.h"
#include "../../scene/Referenceable.h"
#include "../../scene/Cubemap.h"
#include "../RenderMode.h"

class RenderTexture;
class Camera;
class Model;

class CubemapController : public RenderController
{
protected:
	std::vector<RenderTexture*> m_render_textures;
	CumulativeTexture m_cumulative_texture;

	int m_frame_counter;

	Cubemap* m_cubemap;
	Camera* m_camera;

	void DerivedClassConstructedEvent(); //this MUST be called by the derived constructor

	virtual RenderTexture* GenerateRenderTexture(int layer) const = 0;
	virtual bool RepeatingConfigureRenderTexture(RenderTexture* render_texture) const = 0;

public:
	CubemapController(Engine* engine, RenderTextureReference reference);
	CubemapController(const CubemapController&) = delete;
	CubemapController& operator=(const CubemapController&) = delete;
	CubemapController(CubemapController&&) = delete;
	CubemapController& operator=(CubemapController&&) = delete;
	virtual ~CubemapController();

	void Render() override;
	RenderTextureGroup GetRenderTexture() const override;

	virtual CubemapType GetCubemapType() const = 0;

	virtual double GetRenderGroup() const = 0;
	virtual RenderControllerType GetType() const = 0;
};