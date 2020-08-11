#include "SceneApproximation.h"

SceneApproximation::SceneApproximation()
{
}

void SceneApproximation::AddOBB(OrientedBoundingBox obb)
{
	this->m_obbs.push_back(obb);
}

int SceneApproximation::NumOBBs()
{
	return (int)this->m_obbs.size();
}
