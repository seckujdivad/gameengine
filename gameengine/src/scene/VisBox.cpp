#include <wx/wxprec.h>
#include "VisBox.h"

VisBox::VisBox() : Nameable(), Rotatable(), Positionable(), Scalable()
{
}

VisBox::~VisBox()
{
}

void VisBox::SetOBBBias(glm::vec3 obb_bias)
{
	this->m_obb_bias = obb_bias;
}

glm::vec3 VisBox::GetOBBBias()
{
	return this->m_obb_bias;
}

std::vector<Model*> VisBox::GetPotentiallyVisibleModels()
{
	std::vector<Model*> output;
	for (size_t i = 0; i < this->m_pvs.size(); i++)
	{
		std::vector<Model*> visbox_models = this->m_pvs.at(i)->GetMemberModels();
		for (size_t j = 0; j < visbox_models.size(); j++)
		{
			output.push_back(visbox_models.at(j));
		}
	}
	return output;
}

std::vector<Model*> VisBox::GetMemberModels()
{
	return this->m_members;
}

void VisBox::AddMemberModel(Model* model)
{
	this->m_members.push_back(model);
}

void VisBox::RemoveMemberModel(Model* model)
{
	int index = -1;
	for (size_t i = 0; i < this->m_members.size(); i++)
	{
		if (this->m_members.at(i) == model)
		{
			index = i;
		}
	}

	if (index != -1)
	{
		this->m_members.erase(this->m_members.begin() + index);
	}
}

void VisBox::GeneratePotentiallyVisibleSet(std::vector<VisBox*> visboxes)
{
}

void VisBox::AddPotentiallyVisible(VisBox* visbox)
{
	this->m_pvs.push_back(visbox);
}
