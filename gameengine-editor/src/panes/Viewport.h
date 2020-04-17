#pragma once

#include <wx/wxprec.h>
#include <wx/wx.h>

#include "Pane.h"
#include "../PaneHost.h"

class PaneHost;

class Viewport : public Pane
{
private:
	wxBoxSizer* m_sizer;

public:
	Viewport(PaneHost* parent);

	std::string GetDisplayName();
};