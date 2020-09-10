#include "VisBox.h"

#include "model/Model.h"

VisBox::VisBox() : OrientedBoundingBox()
{
}

std::set<Model*> VisBox::GetPotentiallyVisibleModels() const
{
	std::set<Model*> output = this->m_members;

	for (VisBox* visbox : this->m_pvs)
	{
		std::set<Model*> member_models = visbox->GetMemberModels();
		output.insert(member_models.begin(), member_models.end());
	}
	
	return output;
}

std::set<Model*> VisBox::GetMemberModels() const
{
	return this->m_members;
}

void VisBox::AddMemberModel(Model* model)
{
	this->m_members.insert(model);
}

void VisBox::RemoveMemberModel(Model* model)
{
	this->m_members.erase(model);
}

void VisBox::AddPotentiallyVisible(VisBox* visbox)
{
	this->m_pvs.insert(visbox);
}

void VisBox::RemovePotentiallyVisible(VisBox* visbox)
{
	this->m_pvs.erase(visbox);
}
