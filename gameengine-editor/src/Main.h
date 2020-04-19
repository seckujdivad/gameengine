#pragma once

#include <wx/wxprec.h>
#include <wx/wx.h>
#include <wx/gbsizer.h>

#include <string>

#include "PaneHost.h"
#include "panes/Viewport.h"
#include "panes/Lights.h"

class PaneHost;

class Main : public wxFrame
{
private:
	wxGridBagSizer* m_sizer;
	PaneHost* m_panehost;
	wxToolBar* m_toolbar;

	void tlbr_btn_AddPane_clicked(wxCommandEvent& evt);

public:
	Main();
	~Main();
};