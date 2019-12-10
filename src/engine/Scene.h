#pragma once

#include <vector>

#include "Model.h"
#include "render/EngineCanvas.h"

class Scene
{
private:
	std::vector<Model> m_models;
	
public:
	Scene();
	~Scene();

	void Render(EngineCanvas* canvas);
};