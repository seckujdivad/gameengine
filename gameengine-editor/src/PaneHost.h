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

	Main* m_parent;

public:
	PaneHost(Main* parent);
	~PaneHost();

	template <class T>
	void AddPane(int direction = 16);
};

template<class T>
inline void PaneHost::AddPane(int direction)
{
	static_assert(std::is_base_of<Pane, T>::value, "Supplied class must inherit from Pane");

	Pane* new_pane = (Pane*)new T(this);
	this->m_panes.push_back(new_pane);

	wxAuiPaneInfo info;
	info = info.CaptionVisible(true);
	info = info.MinSize(new_pane->GetBestSize());
	info = info.Caption(new_pane->GetDisplayName());
	info = info.Direction(direction);

	this->m_aui_manager.AddPane(new_pane, info);

	this->m_aui_manager.Update();
}
