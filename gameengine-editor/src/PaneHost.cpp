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

int PaneHost::GetPaneIndex(int pane_id)
{
	for (int i = 0; i < (int)this->m_panes.size(); i++)
	{
		if (this->m_panes.at(i)->GetPaneID() == pane_id)
		{
			return i;
		}
	}
	return -1;
}

void PaneHost::AddPane(Pane* pane, int direction, bool docked)
{
	pane->SetPaneID(m_pane_id_counter);
	pane->Bind(wxEVT_SIZE, &PaneHost::PaneResizeHandler, this);

	m_pane_id_counter++;
	this->m_panes.push_back(pane);

	wxAuiPaneInfo info;
	info = info.CaptionVisible(true);
	info = info.Caption(pane->GetDisplayName());
	info = info.Direction(direction);
	info = info.Name(std::to_string(pane->GetPaneID()));

	wxSize best_size = pane->GetBestSize();
	info = info.MinSize(best_size);
	info = info.FloatingSize(wxSize(best_size.x, best_size.y + 30)); //the header is included in this size. it appears to be 30px high on windows

	this->m_panes_docked.push_back(docked);
	if (docked)
	{
		info = info.Dock();
	}
	else
	{
		info = info.Float();
	}

	this->m_aui_manager.AddPane(pane, info);

	this->m_aui_manager.Update();
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