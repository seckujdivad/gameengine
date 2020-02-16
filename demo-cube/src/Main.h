#pragma once

#include <wx/wxprec.h>
#include <wx/wx.h>
#include <wx/gbsizer.h>
#include <wx/spinctrl.h>

#include "GLComponents.h"
#include "render/EngineCanvas.h"
#include "Engine.h"

class Main : public wxFrame
{
private:
	wxGridBagSizer* m_sizer;
	EngineCanvas* m_glcanvas;
	wxButton* m_btn_render;
	wxSpinCtrl* m_spn_zrot;

	Scene* m_scene;

	void btn_render_OnClick(wxCommandEvent& evt);
	void spn_zrot_OnChange(wxCommandEvent& evt);

public:
	Main();
	~Main();
};