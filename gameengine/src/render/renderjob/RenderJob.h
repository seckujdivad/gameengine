#pragma once

class RenderJobFactory;

class RenderJob
{
private:
	RenderJobFactory* m_factory;

public:
	RenderJob(RenderJobFactory* factory);
	virtual ~RenderJob();

	RenderJobFactory* GetFactory() const;

	virtual bool Render() = 0;
};