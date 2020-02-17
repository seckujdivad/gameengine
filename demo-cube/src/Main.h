#pragma once

#include <wx/wxprec.h>
#include <wx/wx.h>
#include <wx/gbsizer.h>

#include "GLComponents.h"
#include "render/EngineCanvas.h"
#include "Engine.h"

class Main : public wxFrame
{
private:
	wxGridBagSizer* m_sizer;
	EngineCanvas* m_glcanvas;
	wxButton* m_btn_render;
	wxSlider* m_sld_xrot;
	wxSlider* m_sld_yrot;
	wxSlider* m_sld_zrot;

	Scene* m_scene;

	void btn_render_OnClick(wxCommandEvent& evt);
	void sld_xrot_OnChange(wxCommandEvent& evt);
	void sld_yrot_OnChange(wxCommandEvent& evt);
	void sld_zrot_OnChange(wxCommandEvent& evt);

public:
	Main();
	~Main();
};