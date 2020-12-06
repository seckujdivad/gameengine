#pragma once

#include <wx/frame.h>

#include "VectorCtrl.h"

class wxGridBagSizer;
class wxButton;
class wxListBox;
class wxStaticText;

class Engine;
class EngineCanvas;
class Scene;
class Camera;
class Model;

class Main : public wxFrame
{
private:
	Engine* m_engine;
	Scene* m_scene;
	Camera* m_camera;

	Model* m_model_selected = nullptr;

	wxGridBagSizer* m_sizer;
	
	EngineCanvas* m_glcanvas;
	wxListBox* m_lb_models;

	VectorCtrl* m_vct_position;
	VectorCtrl* m_vct_rotation;
	VectorCtrl* m_vct_scale;

	wxStaticText* m_stxt_position;
	wxStaticText* m_stxt_rotation;
	wxStaticText* m_stxt_scale;

	void lb_models_OnSelection(wxCommandEvent& evt);
	void lb_models_OnChar(wxKeyEvent& evt);

	void vct_position_OnChange(VectorCtrlEvent& evt);
	void vct_rotation_OnChange(VectorCtrlEvent& evt);
	void vct_scale_OnChange(VectorCtrlEvent& evt);

public:
	Main();
	~Main();

	void SetModel(Model* model);
};