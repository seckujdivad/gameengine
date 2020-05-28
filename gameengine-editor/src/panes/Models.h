#pragma once

#include <wx/wx.h>
#include <wx/gbsizer.h>

#include "Pane.h"
#include "../PaneHost.h"

class Models : public Pane
{
private:
	wxGridBagSizer* m_sizer;

	wxListBox* m_lb_models;

public:
	Models(PaneHost* parent);
	~Models();

	std::string GetDisplayName();

	void SceneChangedEvent(Scene* scene);
};