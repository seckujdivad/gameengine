#pragma once

#include <unordered_set>

#include "../../scene/Referenceable.h"
#include "../rendertarget/RenderTextureData.h"

class Engine;

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

	virtual void Render() = 0;
	virtual void PostRender();

	virtual RenderTextureGroup GetRenderTexture() const = 0;
	virtual RenderControllerType GetType() const = 0;

	virtual std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const = 0;
	virtual bool IsEssentialDraw() const;
};