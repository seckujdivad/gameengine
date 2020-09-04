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
private:
	std::vector<RenderTexture*> m_render_textures;
	CumulativeTexture m_cumulative_texture;

	Camera* m_camera;

	int m_frame_counter;

protected:
	Cubemap* m_cubemap;

	void DerivedClassConstructedEvent(); //this MUST be called by the derived constructor

	virtual RenderTextureInfo GetRenderTextureInfo() const = 0;
	virtual RenderMode GetRenderMode() const = 0;
	virtual void InitialConfigureRenderTexture(RenderTexture* render_texture) const = 0;
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