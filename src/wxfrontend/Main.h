#pragma once

#include <wx/wx.h>
#include <wx/glcanvas.h>
#include <wx/gbsizer.h>

#include "../engine/render/opengl/EngineCanvas.h"


class Main : public wxFrame
{
private:
	wxGridBagSizer* m_sizer;
	EngineCanvas* m_glcanvas;
	wxButton* m_btn_render;

	void btn_render_OnClick(wxCommandEvent& evt);

public:
	Main();
	~Main();
};