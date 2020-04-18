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
	int GetPaneIndex(int pane_id);
	
	void AddPane(Pane* pane, int direction, bool docked);

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
	
	this->AddPane(new_pane, direction, docked);
}
