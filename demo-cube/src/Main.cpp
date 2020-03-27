#include <wx/wxprec.h>
#include "Main.h"

Main::Main() : wxFrame(nullptr, wxID_ANY, "Render Test", wxPoint(30, 30), wxSize(800, 600))
{
	this->SetBackgroundColour(wxColor(238, 238, 238));
	this->SetMinSize(wxSize(500, 400));

	//make sizer
	this->m_sizer = new wxGridBagSizer(0, 0);
	this->m_sizer->SetFlexibleDirection(wxBOTH);
	this->m_sizer->SetNonFlexibleGrowMode(wxFLEX_GROWMODE_SPECIFIED);

	//create glcanvas
	wxGLAttributes args;
	args.PlatformDefaults().Depth(24).Stencil(8).RGBA().DoubleBuffer().EndList();

	this->m_glcanvas = new EngineCanvas(this, wxID_ANY, args);
	this->m_sizer->Add(this->m_glcanvas, wxGBPosition(0, 0), wxGBSpan(1, 3), wxEXPAND | wxALL);

	//create rest of ui

	//create attribute modifications
	std::vector<std::string> attr_names = {
		"Rotate X",
		"Rotate Y",
		"Rotate Z",
		"Translate X",
		"Translate Y",
		"Translate Z",
		"Scale X",
		"Scale Y",
		"Scale Z"
	};

	wxSlider* current_slider;
	wxStaticText* current_text;
	int min, max;
	for (size_t i = 0; i < attr_names.size(); i++)
	{
		if (attr_names.at(i).substr(0, 7) == "Rotate ")
		{
			min = 0;
			max = 360;
		}
		else if (attr_names.at(i).substr(0, 10) == "Translate ")
		{
			min = -20;
			max = 20;
		}
		else if (attr_names.at(i).substr(0, 6) == "Scale ")
		{
			min = 0;
			max = 10;
		}

		current_text = new wxStaticText(this, wxID_ANY, attr_names.at(i));
		
		current_slider = new wxSlider(this, wxID_ANY, 0, min, max);
		current_slider->Bind(wxEVT_SLIDER, &Main::sld_OnChange, this);

		this->m_sizer->Add(current_slider, wxGBPosition(i + 1, 2), wxGBSpan(1, 1), wxEXPAND | wxALL);
		this->m_sizer->Add(current_text, wxGBPosition(i + 1, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);

		this->m_mdl_sliders.push_back(current_slider);
		this->m_mdl_slider_lookup.insert(std::pair<int, std::string>(current_slider->GetId(), attr_names.at(i)));
	}

	//make render button
	this->m_btn_render = new wxButton(this, wxID_ANY, wxString("Render"));
	this->m_btn_render->Bind(wxEVT_BUTTON, &Main::btn_render_OnClick, this);
	this->m_sizer->Add(this->m_btn_render, wxGBPosition(attr_names.size(), 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	//make list to hold all models
	this->m_lb_models = new wxListBox(this, wxID_ANY);
	this->m_lb_models->Bind(wxEVT_LISTBOX, &Main::lb_models_OnSelection, this);
	this->m_sizer->Add(this->m_lb_models, wxGBPosition(1, 0), wxGBSpan(attr_names.size() - 1, 1), wxEXPAND | wxALL);

	this->m_model_selection_index = -1;

	//load scene
	std::string scene_path = "resources";
	this->m_scene = InitialiseScene(this->m_scene_path, this->m_scene_filename);
	this->m_glcanvas->SetScene(this->m_scene);
	this->m_scene->PushUniforms();

	this->SetTitle("Render Test: viewing " + this->m_scene->GetIdentifier() + " (" + this->m_scene_filename + ")");

	//load model names
	for (size_t i = 0; i < this->m_scene->models.size(); i++)
	{
		this->m_lb_models->Append(this->m_scene->models.at(i)->GetIdentifier());
	}

	//start mainloop
	this->Bind(wxEVT_TIMER, &Main::Mainloop, this);
	this->m_timer_mainloop = new wxTimer(this);
	this->m_timer_mainloop->Start(30);

	//final layout configuration
	this->m_sizer->AddGrowableRow(0);
	this->m_sizer->AddGrowableCol(0);
	this->m_sizer->AddGrowableCol(2);

	this->SetSizer(this->m_sizer);
	this->Centre(wxBOTH);
	this->Layout();
}

Main::~Main()
{
	this->m_timer_mainloop->Stop();
	delete this->m_scene;
	delete this->m_timer_mainloop;
}

void Main::btn_render_OnClick(wxCommandEvent& evt)
{
	this->m_glcanvas->Render();
	evt.Skip();
}

void Main::sld_OnChange(wxCommandEvent& evt)
{
	if (this->m_model_selection_index != -1)
	{
		std::string slider_name = this->m_mdl_slider_lookup.at(evt.GetId());
		wxSlider* slider = (wxSlider*)evt.GetEventObject();

		if (slider_name == "Rotate X")
		{
			this->m_scene->models.at(this->m_model_selection_index)->SetRotation(0, slider->GetValue());
		}
		else if (slider_name == "Rotate Y")
		{
			this->m_scene->models.at(this->m_model_selection_index)->SetRotation(1, slider->GetValue());
		}
		else if (slider_name == "Rotate Z")
		{
			this->m_scene->models.at(this->m_model_selection_index)->SetRotation(2, slider->GetValue());
		}
		else if (slider_name == "Translate X")
		{
			this->m_scene->models.at(this->m_model_selection_index)->SetPosition(0, slider->GetValue());
		}
		else if (slider_name == "Translate Y")
		{
			this->m_scene->models.at(this->m_model_selection_index)->SetPosition(1, slider->GetValue());
		}
		else if (slider_name == "Translate Z")
		{
			this->m_scene->models.at(this->m_model_selection_index)->SetPosition(2, slider->GetValue());
		}
		else if (slider_name == "Scale X")
		{
			this->m_scene->models.at(this->m_model_selection_index)->SetScale(0, slider->GetValue());
		}
		else if (slider_name == "Scale Y")
		{
			this->m_scene->models.at(this->m_model_selection_index)->SetScale(1, slider->GetValue());
		}
		else if (slider_name == "Scale Z")
		{
			this->m_scene->models.at(this->m_model_selection_index)->SetScale(2, slider->GetValue());
		}

		this->m_glcanvas->Render();
	}
	
	evt.Skip();
}

void Main::lb_models_OnSelection(wxCommandEvent& evt)
{
	this->m_model_selection_index = this->m_lb_models->GetSelection();

	if (this->m_model_selection_index != -1)
	{
		wxSlider* slider;
		for (size_t i = 0; i < this->m_mdl_sliders.size(); i++)
		{
			slider = this->m_mdl_sliders.at(i);
			std::string slider_name = this->m_mdl_slider_lookup.at(slider->GetId());

			if (slider_name == "Rotate X")
			{
				slider->SetValue(this->m_scene->models.at(this->m_model_selection_index)->GetRotation(0));
			}
			else if (slider_name == "Rotate Y")
			{
				slider->SetValue(this->m_scene->models.at(this->m_model_selection_index)->GetRotation(1));
			}
			else if (slider_name == "Rotate Z")
			{
				slider->SetValue(this->m_scene->models.at(this->m_model_selection_index)->GetRotation(2));
			}
			else if (slider_name == "Translate X")
			{
				slider->SetValue(this->m_scene->models.at(this->m_model_selection_index)->GetPosition(0));
			}
			else if (slider_name == "Translate Y")
			{
				slider->SetValue(this->m_scene->models.at(this->m_model_selection_index)->GetPosition(1));
			}
			else if (slider_name == "Translate Z")
			{
				slider->SetValue(this->m_scene->models.at(this->m_model_selection_index)->GetPosition(2));
			}
			else if (slider_name == "Scale X")
			{
				slider->SetValue(this->m_scene->models.at(this->m_model_selection_index)->GetScale(0));
			}
			else if (slider_name == "Scale Y")
			{
				slider->SetValue(this->m_scene->models.at(this->m_model_selection_index)->GetScale(1));
			}
			else if (slider_name == "Scale Z")
			{
				slider->SetValue(this->m_scene->models.at(this->m_model_selection_index)->GetScale(2));
			}
		}
	}
}

void Main::Mainloop(wxTimerEvent& evt) //this method is called repeatedly by the window manager
{
	this->m_timer_mainloop->Stop();

	bool redraw_required = false;

	//poll keys
	if (wxGetKeyState(wxKeyCode('W')))
	{
		this->m_scene->GetActiveCamera()->SetPosition(0, this->m_scene->GetActiveCamera()->GetPosition(0) + 0.5);
		redraw_required = true;
	}
	if (wxGetKeyState(wxKeyCode('S')))
	{
		this->m_scene->GetActiveCamera()->SetPosition(0, this->m_scene->GetActiveCamera()->GetPosition(0) - 0.5);
		redraw_required = true;
	}
	if (wxGetKeyState(wxKeyCode('D')))
	{
		this->m_scene->GetActiveCamera()->SetPosition(1, this->m_scene->GetActiveCamera()->GetPosition(1) - 0.5);
		redraw_required = true;
	}
	if (wxGetKeyState(wxKeyCode('A')))
	{
		this->m_scene->GetActiveCamera()->SetPosition(1, this->m_scene->GetActiveCamera()->GetPosition(1) + 0.5);
		redraw_required = true;
	}
	if (wxGetKeyState(WXK_CONTROL))
	{
		this->m_scene->GetActiveCamera()->SetPosition(2, this->m_scene->GetActiveCamera()->GetPosition(2) + 0.5);
		redraw_required = true;
	}
	if (wxGetKeyState(WXK_SHIFT))
	{
		this->m_scene->GetActiveCamera()->SetPosition(2, this->m_scene->GetActiveCamera()->GetPosition(2) - 0.5);
		redraw_required = true;
	}

	if (redraw_required)
	{
		this->m_glcanvas->Render();
	}

	this->m_timer_mainloop->Start();
	evt.Skip();
}