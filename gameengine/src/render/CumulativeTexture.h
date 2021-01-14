#pragma once

#include <vector>
#include <optional>
#include <functional>

class RenderTarget;
class RenderJobFactory;
class Model;

class CumulativeTexture
{
public:
	using FetchModelsFunction = std::function<std::vector<Model*>(int layer)>;

private:
	std::vector<RenderJobFactory*> m_factories;

	FetchModelsFunction m_fetch_models_function;

public:
	CumulativeTexture(std::vector<RenderJobFactory*> factories);

	void Render(int index = 0, bool continuous_draw = false) const;

	RenderTarget* GetOutput() const;

	void SetFetchModelsFunction(FetchModelsFunction func);
};