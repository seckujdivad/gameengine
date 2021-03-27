#include "VisBox.h"

#include "model/Model.h"

VisBox::VisBox() : OrientedBoundingBox()
{
}

std::set<std::shared_ptr<Model>> VisBox::GetPotentiallyVisibleModels() const
{
	std::set<std::shared_ptr<Model>> output = this->m_members;

	for (VisBox* visbox : this->m_pvs)
	{
		std::set<std::shared_ptr<Model>> member_models = visbox->GetMemberModels();
		output.insert(member_models.begin(), member_models.end());
	}
	
	return output;
}

std::set<std::shared_ptr<Model>> VisBox::GetMemberModels() const
{
	return this->m_members;
}

void VisBox::AddMemberModel(std::shared_ptr<Model> model)
{
	this->m_members.insert(model);
}

void VisBox::RemoveMemberModel(std::shared_ptr<Model> model)
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
