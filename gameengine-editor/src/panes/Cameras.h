#pragma once

#include <wx/wx.h>
#include <wx/gbsizer.h>
#include <wx/listbox.h>
#include <wx/button.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include <string>

#include "scene/Scene.h"

#include "Pane.h"
#include "../PaneHost.h"

class Cameras : public Pane
{
private:
	wxGridBagSizer* m_sizer;

	wxListBox* m_lb_cameras;

public:
	Cameras(PaneHost* parent);

	std::string GetDisplayName();
	void SceneChangedEvent(Scene* scene);
};