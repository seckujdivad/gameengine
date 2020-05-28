#pragma once

#include <wx/wxprec.h>
#include <wx/wx.h>
#include <wx/gbsizer.h>

#include <cstdlib>

#include "render/EngineCanvas.h"

#include "Pane.h"
#include "../PaneHost.h"

class PaneHost;

class Viewport : public Pane
{
private:
	wxGridBagSizer* m_sizer;

	EngineCanvas* m_glcanvas;

public:
	Viewport(PaneHost* parent);

	std::string GetDisplayName();

	void SceneChangedEvent(Scene* scene);
};