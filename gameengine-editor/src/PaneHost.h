#pragma once

#include <wx/wxprec.h>
#include <wx/wx.h>
#include <wx/aui/aui.h>

#include "Main.h"

class Pane;
class Main;

class PaneHost : public wxPanel
{
private:
	wxAuiManager m_aui_manager;
	std::vector<Pane*> m_panes;
	std::vector<bool> m_panes_docked;
	int m_pane_id_counter = 0;

	Main* m_parent;

	int GetPaneIndex(Pane* pane);

public:
	PaneHost(Main* parent);
	~PaneHost();

	template <class T>
	void AddPane(int direction = 16, bool docked = true);

	wxAuiPaneInfo GetPaneInfo(int pane_id);
	wxAuiPaneInfo GetPaneInfo(std::string pane_id);
	wxAuiPaneInfo GetPaneInfo(Pane* pane);

	void PaneResizeHandler(wxSizeEvent& evt);
};

template<class T>
inline void PaneHost::AddPane(int direction, bool docked)
{
	static_assert(std::is_base_of<Pane, T>::value, "Supplied class must inherit from Pane");

	Pane* new_pane = (Pane*)new T(this);
	new_pane->SetPaneID(m_pane_id_counter);
	new_pane->Bind(wxEVT_SIZE, &PaneHost::PaneResizeHandler, this);

	m_pane_id_counter++;
	this->m_panes.push_back(new_pane);

	wxAuiPaneInfo info;
	info = info.CaptionVisible(true);
	info = info.MinSize(new_pane->GetBestSize());
	info = info.Caption(new_pane->GetDisplayName());
	info = info.Direction(direction);
	info = info.Name(std::to_string(new_pane->GetPaneID()));
	
	this->m_panes_docked.push_back(docked);
	if (docked)
	{
		info = info.Dock();
	}
	else
	{
		info = info.Float();
	}
	
	this->m_aui_manager.AddPane(new_pane, info);

	this->m_aui_manager.Update();
}
