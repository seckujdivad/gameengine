#pragma once

#include "../../Engine.h"
#include "../Renderable.h"
#include "../RenderTexture.h"
#include "../../scene/Referenceable.h"

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

	virtual void Render() = 0;
	virtual RenderTextureGroup GetRenderTexture() = 0;
	virtual double GetRenderGroup() = 0;
	virtual RenderControllerType GetType() = 0;
};