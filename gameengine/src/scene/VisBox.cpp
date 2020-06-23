#include <wx/wxprec.h>
#include "VisBox.h"

VisBox::VisBox(EventManager* evtman) : Nameable(evtman), OrientedBoundingBox(evtman), EventEmitter(evtman)
{
}

VisBox::~VisBox()
{
}

std::unordered_set<Model*, HashPointer<Model>> VisBox::GetPotentiallyVisibleModels()
{
	std::unordered_set<Model*, HashPointer<Model>> output = this->m_members;

	for (auto it = this->m_pvs.begin(); it != this->m_pvs.end(); it++)
	{
		VisBox* visbox = *it;
		std::unordered_set<Model*, HashPointer<Model>> member_models = visbox->GetMemberModels();
		output.insert(member_models.begin(), member_models.end());
	}
	
	return output;
}

std::unordered_set<Model*, HashPointer<Model>> VisBox::GetMemberModels()
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