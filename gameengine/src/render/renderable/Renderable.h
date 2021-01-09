#pragma once

#include <vector>
#include <tuple>

class Model;

class Renderable
{
public:
	Renderable();
	virtual ~Renderable();

	virtual void Render(std::vector<Model*> models, bool continuous_draw = false) = 0;

	virtual std::tuple<int, int> GetOutputSize() const = 0;
	virtual bool SetOutputSize(std::tuple<int, int> dimensions) = 0;
};