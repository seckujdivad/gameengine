#pragma once

#include <vector>
#include <optional>
#include <functional>

#include "Renderable.h"

class RenderTarget;
class Renderer;
class Model;

class CumulativeTexture : Renderable
{
public:
	using FetchModelsFunction = std::function<std::vector<Model*>(int layer)>;

private:
	std::vector<Renderer*> m_renderers;

	FetchModelsFunction m_fetch_models_function;

public:
	CumulativeTexture(std::vector<Renderer*> renderers);

	void Render(std::vector<Model*> models, bool continuous_draw = false) override;
	void Render(int index = 0, bool continuous_draw = false) const;

	void SetFetchModelsFunction(FetchModelsFunction func);

	RenderTarget* GetOutput() const;

	std::tuple<int, int> GetOutputSize() const override;
	bool SetOutputSize(std::tuple<int, int> dimensions) override;
};