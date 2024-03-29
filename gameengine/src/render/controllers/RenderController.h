#pragma once

#include <unordered_set>
#include <memory>
#include <ctime>

#include "../../scene/Referenceable.h"

class Engine;
class RenderTextureGroup;

enum class RenderControllerType //runtime type determination is essential
{
	EngineCanvas,
	Reflection,
	Shadow,
	Skybox
};

class RenderController : public Referenceable<RenderTextureReference>
{
protected:
	Engine* m_engine;

public:
	RenderController(Engine* engine, RenderTextureReference reference);
	virtual ~RenderController();

	virtual void Render(std::clock_t draw_time, bool continuous_draw = false) = 0;
	virtual void PostRender();

	virtual std::shared_ptr<RenderTextureGroup> GetRenderTexture() const = 0;
	virtual RenderControllerType GetType() const = 0;

	virtual std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const = 0;
	virtual bool IsEssentialDraw() const;
};