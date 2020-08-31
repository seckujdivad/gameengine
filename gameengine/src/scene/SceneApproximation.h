#pragma once

#include <vector>

#include "OrientedBoundingBox.h"

class SceneApproximation
{
private:
	std::vector<OrientedBoundingBox> m_obbs;

public:
	SceneApproximation();

	void AddOBB(OrientedBoundingBox obb);

	int NumOBBs() const;
};