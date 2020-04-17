#pragma once

#include <wx/wxprec.h>
#include <wx/wx.h>

class PaneHost;

class Pane : public wxPanel
{
private:
	PaneHost* m_parent;

public:
	Pane(PaneHost* parent);
	~Pane();

	virtual std::string GetDisplayName();
};

#include "../PaneHost.h"