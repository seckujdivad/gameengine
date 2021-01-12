#pragma once

#include "../renderable/Renderable.h"

class RenderJobFactory;

class RenderJob : public Renderable
{
private:
	RenderJobFactory* m_factory;

public:
	RenderJob(RenderJobFactory* factory);
	virtual ~RenderJob();

	RenderJobFactory* GetFactory() const;

	std::tuple<int, int> GetOutputSize() const;
	bool SetOutputSize(std::tuple<int, int> dimensions);
};