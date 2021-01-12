#pragma once

#include <memory>
#include <tuple>

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

	std::tuple<int, int> GetOutputSize() const;
	virtual bool SetOutputSize(std::tuple<int, int> dimensions) = 0;
};