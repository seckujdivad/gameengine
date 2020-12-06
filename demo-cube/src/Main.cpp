#include "Main.h"

#include <wx/gbsizer.h>
#include <wx/listbox.h>
#include <wx/stattext.h>

#include "render/EngineCanvas.h"
#include "Engine.h"
#include "loaders/SceneLoader.h"
#include "scene/Scene.h"
#include "scene/model/Model.h"
#include "scene/Camera.h"
#include "generic/std_glm.h"

Main::Main() : wxFrame(nullptr, wxID_ANY, "Render Test", wxDefaultPosition, wxSize(800, 600))
{
	this->SetBackgroundColour(wxColor(238, 238, 238));
	this->SetMinSize(wxSize(500, 400));

	//make sizer
	this->m_sizer = new wxGridBagSizer(0, 0);
	this->m_sizer->SetFlexibleDirection(wxBOTH);
	this->m_sizer->SetNonFlexibleGrowMode(wxFLEX_GROWMODE_SPECIFIED);

	//create glcanvas
	this->m_scene = SceneFromJSON("resources", "simplescene.json");

	this->m_engine = new Engine(this, this->m_scene);
	this->m_engine->SetDebugMessageLevel(std::vector({
		Engine::DebugMessageConfig({ GL_DONT_CARE, GL_DONT_CARE, GL_DEBUG_SEVERITY_NOTIFICATION, false })
		}));

	this->m_camera = new Camera();
	this->m_camera->SetFOV(90.0);
	this->m_camera->SetRotation(90.0, 0.0, 0.0);
	this->m_camera->SetClips({ 0.1, 100.0 });

	std::vector<RenderableConfig> canvas_layers;

	RenderableConfig normal_config = { RenderMode::Normal, RenderableConfig::Normal() };
	canvas_layers.push_back(normal_config);

	this->m_glcanvas = this->m_engine->GenerateNewCanvas(canvas_layers, wxID_ANY, this);
	this->m_glcanvas->SetControlledCamera(this->m_camera);
	this->m_glcanvas->SetMouselook(true);
	this->m_glcanvas->SetKeyboardMove(true);
	this->m_glcanvas->SetRenderLoop(true);

	this->m_sizer->Add(this->m_glcanvas, wxGBPosition(0, 0), wxGBSpan(4, 1), wxEXPAND | wxALL);

	//make list to display all model identifiers
	this->m_lb_models = new wxListBox(this, wxID_ANY);
	this->m_lb_models->Bind(wxEVT_LISTBOX, &Main::lb_models_OnSelection, this);
	this->m_lb_models->Bind(wxEVT_CHAR, &Main::lb_models_OnChar, this);
	this->m_sizer->Add(this->m_lb_models, wxGBPosition(0, 1), wxGBSpan(1, 2), wxEXPAND | wxALL);

	for (Model* model : this->m_scene->GetModels())
	{
		this->m_lb_models->Append(model->GetIdentifier());
	}

	//make vector controls for modifying models
	VectorCtrlConfig vct_cfg;
	vct_cfg.num_fields = 3;

	vct_cfg.inc = 1.0;
	this->m_vct_position = new VectorCtrl(this, wxID_ANY, vct_cfg);
	this->m_vct_position->Bind(EVT_VCTRCTRL_CHANGED, &Main::vct_position_OnChange, this);
	this->m_sizer->Add(this->m_vct_position, wxGBPosition(1, 2), wxGBSpan(1, 1), wxEXPAND | wxALL);

	vct_cfg.inc = 15.0;
	this->m_vct_rotation = new VectorCtrl(this, wxID_ANY, vct_cfg);
	this->m_vct_rotation->Bind(EVT_VCTRCTRL_CHANGED, &Main::vct_rotation_OnChange, this);
	this->m_sizer->Add(this->m_vct_rotation, wxGBPosition(2, 2), wxGBSpan(1, 1), wxEXPAND | wxALL);

	vct_cfg.inc = 1.0;
	this->m_vct_scale = new VectorCtrl(this, wxID_ANY, vct_cfg);
	this->m_vct_scale->Bind(EVT_VCTRCTRL_CHANGED, &Main::vct_scale_OnChange, this);
	this->m_sizer->Add(this->m_vct_scale, wxGBPosition(3, 2), wxGBSpan(1, 1), wxEXPAND | wxALL);

	//make labels for vector controls
	this->m_stxt_position = new wxStaticText(this, wxID_ANY, "Position");
	this->m_sizer->Add(this->m_stxt_position, wxGBPosition(1, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_stxt_rotation = new wxStaticText(this, wxID_ANY, "Rotation");
	this->m_sizer->Add(this->m_stxt_rotation, wxGBPosition(2, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_stxt_scale = new wxStaticText(this, wxID_ANY, "Scale");
	this->m_sizer->Add(this->m_stxt_scale, wxGBPosition(3, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);

	//set window title
	this->SetTitle("Render Test: viewing " + this->m_scene->GetIdentifier());

	this->SetModel(nullptr);

	//final layout configuration
	this->m_sizer->AddGrowableRow(0);
	this->m_sizer->AddGrowableCol(0);

	this->SetSizer(this->m_sizer);
	this->Centre(wxBOTH);
	this->Layout();
}

Main::~Main()
{
	delete this->m_engine;
	delete this->m_scene;
	delete this->m_camera;
}

void Main::SetModel(Model* model)
{
	if (this->m_model_selected != nullptr)
	{
		this->m_model_selected->SetCurrentWireframeIndex(0);
	}
	this->m_model_selected = model;

	if (model == nullptr)
	{
		this->m_lb_models->SetSelection(wxNOT_FOUND);

		this->m_vct_position->SetValues({ 0.0, 0.0, 0.0 });
		this->m_vct_rotation->SetValues({ 0.0, 0.0, 0.0 });
		this->m_vct_scale->SetValues({ 0.0, 0.0, 0.0 });

		this->m_vct_position->Disable();
		this->m_vct_rotation->Disable();
		this->m_vct_scale->Disable();
	}
	else
	{
		model->SetCurrentWireframeIndex(1);
			
		std::vector<Model*> models = this->m_scene->GetModels();
		for (size_t i = 0; i < models.size(); i++)
		{
			if (models.at(i) == model)
			{
				this->m_lb_models->SetSelection(static_cast<int>(i));
			}
		}

		this->m_vct_position->Enable();
		this->m_vct_rotation->Enable();
		this->m_vct_scale->Enable();

		this->m_vct_position->SetValues(GetSTDVector(this->m_model_selected->GetPosition()));
		this->m_vct_rotation->SetValues(GetSTDVector(this->m_model_selected->GetRotation()));
		this->m_vct_scale->SetValues(GetSTDVector(this->m_model_selected->GetScale()));
	}
}

void Main::lb_models_OnSelection(wxCommandEvent& evt)
{
	int selection_index = this->m_lb_models->GetSelection();
	if (selection_index != wxNOT_FOUND)
	{
		std::vector<Model*> models = this->m_scene->GetModels();
		this->SetModel(models.at(selection_index));
	}
}

void Main::lb_models_OnChar(wxKeyEvent& evt)
{
	if (evt.GetKeyCode() == WXK_ESCAPE)
	{
		this->SetModel(nullptr);
	}
	evt.Skip();
}

void Main::vct_position_OnChange(VectorCtrlEvent& evt)
{
	if (this->m_model_selected != nullptr)
	{
		this->m_model_selected->SetPosition(GetGLMVector<3>(this->m_vct_position->GetValues()));
	}
	evt.Skip();
}

void Main::vct_rotation_OnChange(VectorCtrlEvent& evt)
{
	if (this->m_model_selected != nullptr)
	{
		this->m_model_selected->SetRotation(GetGLMVector<3>(this->m_vct_rotation->GetValues()));
	}
	evt.Skip();
}

void Main::vct_scale_OnChange(VectorCtrlEvent& evt)
{
	if (this->m_model_selected != nullptr)
	{
		this->m_model_selected->SetScale(GetGLMVector<3>(this->m_vct_scale->GetValues()));
	}
	evt.Skip();
}
