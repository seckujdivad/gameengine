#include "VisBox.h"

#include "model/Model.h"

VisBox::VisBox() : OrientedBoundingBox()
{
}

std::unordered_set<Model*, HashPointer<Model>> VisBox::GetPotentiallyVisibleModels() const
{
	std::unordered_set<Model*, HashPointer<Model>> output = this->m_members;

	for (VisBox* visbox : this->m_pvs)
	{
		std::unordered_set<Model*, HashPointer<Model>> member_models = visbox->GetMemberModels();
		output.insert(member_models.begin(), member_models.end());
	}
	
	return output;
}

std::unordered_set<Model*, HashPointer<Model>> VisBox::GetMemberModels() const
{
	return this->m_members;
}

void VisBox::AddMemberModel(Model* model)
{
	this->m_members.insert(model);
}

void VisBox::RemoveMemberModel(Model* model)
{
	this->m_members.erase(this->m_members.find(model));
}

void VisBox::AddPotentiallyVisible(VisBox* visbox)
{
	this->m_pvs.insert(visbox);
}