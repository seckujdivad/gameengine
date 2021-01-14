#pragma once

#include <memory>
#include <tuple>
#include <unordered_set>

#include "RenderJob.h"
#include "../../scene/Referenceable.h"

class Engine;
class RenderTarget;

struct RenderJobInitialiser
{
	virtual ~RenderJobInitialiser(); //to ensure RTTI can be used
};

class RenderJobFactory
{
private:
	Engine* m_engine;
	RenderTarget* m_target;

public:
	RenderJobFactory(Engine* engine, RenderTarget* target);
	virtual ~RenderJobFactory();

	Engine* GetEngine() const;
	RenderTarget* GetTarget() const;

	std::tuple<int, int> GetOutputSize() const;
	virtual bool SetOutputSize(std::tuple<int, int> dimensions) = 0;

	virtual std::shared_ptr<RenderJob> GenerateJob(RenderJobInitialiser* initialiser) = 0;

	virtual void CopyFrom(const RenderJobFactory* src) const = 0;
	void CopyTo(const RenderJobFactory* dest) const;

	virtual std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const = 0;
};