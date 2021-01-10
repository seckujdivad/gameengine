#pragma once

#include <memory>

#include "RenderJob.h"

class Engine;

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

public:
	RenderJobFactory(Engine* engine);
	virtual ~RenderJobFactory();

	Engine* GetEngine() const;

	virtual std::shared_ptr<RenderJob> GenerateJob(RenderJobInitialiser* initialiser) = 0;
};