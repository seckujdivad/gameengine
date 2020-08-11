#pragma once

#include <vector>

#include "OrientedBoundingBox.h"
#include "../render/ShaderProgram.h"

class SceneApproximation
{
private:
	std::vector<OrientedBoundingBox> m_obbs;

public:
	SceneApproximation();

	void AddOBB(OrientedBoundingBox obb);

	int NumOBBs();
};