#pragma once

#include <wx/wxprec.h>
#include <wx/wx.h>
#include <wx/gbsizer.h>
#include <wx/listbox.h>

#include <map>

#include "GLComponents.h"
#include "render/EngineCanvas.h"
#include "Engine.h"

class Main : public wxFrame
{
private:
	wxGridBagSizer* m_sizer;
	EngineCanvas* m_glcanvas;
	wxButton* m_btn_render;
	wxListBox* m_lb_models;

	std::string m_scene_path = "resources";
	std::string m_scene_filename = "simplescene.json";

	//model attributes
	std::vector<wxSlider*> m_mdl_sliders;
	std::map<int, std::string> m_mdl_slider_lookup;
	int m_model_selection_index;

	Scene* m_scene;

	void btn_render_OnClick(wxCommandEvent& evt);
	void sld_OnChange(wxCommandEvent& evt);
	void lb_models_OnSelection(wxCommandEvent& evt);

public:
	Main();
	~Main();
};