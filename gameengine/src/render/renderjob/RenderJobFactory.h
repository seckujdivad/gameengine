#pragma once

#include <memory>

#include "RenderJob.h"

class Engine;
class RenderTarget;

enum class RenderJobFactoryType
{
	None
};

struct RenderJobInitialiser
{
	RenderJobFactoryType type = RenderJobFactoryType::None;
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

	virtual std::shared_ptr<RenderJob> GenerateJob(RenderJobInitialiser* initialiser) = 0;
};