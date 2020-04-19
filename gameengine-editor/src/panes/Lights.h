#pragma once

#include <wx/wx.h>
#include <wx/gbsizer.h>

#include "Pane.h"
#include "../PaneHost.h"

class Lights : public Pane
{
private:
	wxGridBagSizer* m_sizer;

public:
	Lights(PaneHost* parent);
	~Lights();

	std::string GetDisplayName();
};