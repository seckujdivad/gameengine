#pragma once

#include <wx/wxprec.h>
#include <wx/wx.h>
#include <wx/gbsizer.h>
#include <wx/listbox.h>

#include <map>

#include "render/EngineCanvas.h"
#include "Engine.h"
#include "loaders/SceneLoader.h"

class Main : public wxFrame
{
private:
	wxGridBagSizer* m_sizer;
	Engine* m_engine;
	EngineCanvas* m_glcanvas;
	wxButton* m_btn_render;
	wxListBox* m_lb_models;

	std::string m_scene_path = "resources";
	std::string m_scene_filename = "simplescene.json";

	//model attributes
	std::vector<wxSlider*> m_mdl_sliders;
	std::map<int, std::string> m_mdl_slider_lookup;
	Model* m_model_selected = nullptr;

	Scene* m_scene;
	Camera* m_camera;

	void btn_render_OnClick(wxCommandEvent& evt);
	void sld_OnChange(wxCommandEvent& evt);
	void lb_models_OnSelection(wxCommandEvent& evt);

public:
	Main();
	~Main();
};