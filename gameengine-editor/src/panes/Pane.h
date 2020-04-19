#pragma once

#include <wx/wxprec.h>
#include <wx/wx.h>
#include <wx/aui/aui.h>

class PaneHost;

class Pane : public wxPanel
{
private:
	PaneHost* m_parent;

	int m_pane_id = -1;

public:
	Pane(PaneHost* parent);
	~Pane();

	virtual std::string GetDisplayName();
	void SetPaneID(int id);
	int GetPaneID();

	wxAuiPaneInfo GetPaneInfo();

	virtual void PaneDockStateChanged(wxAuiPaneInfo info);
};

#include "../PaneHost.h"