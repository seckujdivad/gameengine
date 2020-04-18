#include <wx/wxprec.h>
#include "PaneHost.h"

int PaneHost::GetPaneIndex(Pane* pane)
{
	for (int i = 0; i < (int)this->m_panes.size(); i++)
	{
		if (this->m_panes.at(i) == pane)
		{
			return i;
		}
	}
	return -1;
}

PaneHost::PaneHost(Main* parent) : wxPanel(parent)
{
	this->m_parent = parent;

	this->SetMinSize(wxSize(100, 100));

	this->m_aui_manager.SetManagedWindow(this);

	this->m_aui_manager.Update();
}

PaneHost::~PaneHost()
{
	this->m_aui_manager.UnInit();
}

wxAuiPaneInfo PaneHost::GetPaneInfo(int pane_id)
{
	return this->GetPaneInfo(std::to_string(pane_id));
}

wxAuiPaneInfo PaneHost::GetPaneInfo(std::string pane_id)
{
	return this->m_aui_manager.GetPane(pane_id);
}

wxAuiPaneInfo PaneHost::GetPaneInfo(Pane* pane)
{
	return this->GetPaneInfo(pane->GetPaneID());
}

void PaneHost::PaneResizeHandler(wxSizeEvent& evt)
{
	Pane* pane = (Pane*)evt.GetEventObject();

	wxAuiPaneInfo info = pane->GetPaneInfo();
	int pane_index = this->GetPaneIndex(pane);

	if (info.IsDocked() != this->m_panes_docked.at(pane_index))
	{
		this->m_panes_docked.at(pane_index) = info.IsDocked();
		pane->PaneDockStateChanged(info);
	}

	evt.Skip();
}
