#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "Nameable.h"
#include "Positionable.h"
#include "Rotatable.h"
#include "Scalable.h"
#include "model/Model.h"

class VisBox : public Nameable, public Positionable, public Rotatable, public Scalable
{
private:
	glm::vec3 m_obb_bias = glm::vec3(0.0f, 0.0f, 0.0f);

	std::vector<VisBox*> m_pvs;
	std::vector<Model*> m_members;
	
public:
	VisBox();
	~VisBox();

	void SetOBBBias(glm::vec3 obb_bias);
	glm::vec3 GetOBBBias();

	std::vector<Model*> GetPotentiallyVisibleModels();
	std::vector<Model*> GetMemberModels();
	void AddMemberModel(Model* model);
	void RemoveMemberModel(Model* model);

	void GeneratePotentiallyVisibleSet(std::vector<VisBox*> visboxes);
	void AddPotentiallyVisible(VisBox* visbox);
};