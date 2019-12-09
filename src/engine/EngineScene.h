#pragma once

#include <vector>

#include "EngineModel.h"
#include "render/EngineCanvas.h"

class EngineScene
{
private:
	std::vector<EngineModel> m_models;
	
public:
	EngineScene();
	~EngineScene();

	void Render(EngineCanvas* canvas);
};