#pragma once

#include <wx/wxprec.h>
#include <wx/wx.h>
#include <wx/glcanvas.h>
#include <wx/gbsizer.h>

#include "render/EngineCanvas.h"
//#include "Engine.h"

class Main : public wxFrame
{
private:
	wxGridBagSizer* m_sizer;
	EngineCanvas* m_glcanvas;
	wxButton* m_btn_render;

	//Scene* m_scene;

	void btn_render_OnClick(wxCommandEvent& evt);

public:
	Main();
	~Main();
};