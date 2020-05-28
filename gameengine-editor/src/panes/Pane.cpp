#include <wx/wxprec.h>
#include "Pane.h"

Pane::Pane(PaneHost* parent) : wxPanel(parent)
{
	this->m_parent = parent;
}

Pane::~Pane()
{
}

std::string Pane::GetDisplayName()
{
	return "Pane";
}

void Pane::SetPaneID(int id)
{
	this->m_pane_id = id;
}

int Pane::GetPaneID()
{
	if (this->m_pane_id == -1)
	{
		throw std::runtime_error("No pane ID has been set");
	}
	else
	{
		return this->m_pane_id;
	}
}

wxAuiPaneInfo Pane::GetPaneInfo()
{
	return this->m_parent->GetPaneInfo((Pane*)this);
}

PaneHost* Pane::GetPaneHost()
{
	return this->m_parent;
}

void Pane::PaneDockStateChanged(wxAuiPaneInfo info)
{
}

void Pane::SceneChangedEvent(Scene* scene)
{
}

void Pane::ModelSelectionChangedEvent(Model* model)
{

}
