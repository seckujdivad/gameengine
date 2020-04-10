#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <unordered_set>

#include "Nameable.h"
#include "Positionable.h"
#include "Rotatable.h"
#include "Scalable.h"
#include "model/Model.h"

class VisBox : public Nameable, public Positionable, public Rotatable, public Scalable
{
private:
	glm::vec3 m_obb_bias = glm::vec3(0.0f, 0.0f, 0.0f);

	std::unordered_set<VisBox*> m_pvs;
	std::unordered_set<Model*> m_members;
	
public:
	VisBox();
	~VisBox();

	void SetOBBBias(glm::vec3 obb_bias);
	glm::vec3 GetOBBBias();

	std::unordered_set<Model*> GetPotentiallyVisibleModels();
	std::unordered_set<Model*> GetMemberModels();
	void AddMemberModel(Model* model);
	void RemoveMemberModel(Model* model);
	
	void AddPotentiallyVisible(VisBox* visbox);

	bool PointInOBB(glm::vec3 point);
	bool PointInOBB(float x, float y, float z);
};