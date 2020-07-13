#include "Cameras.h"

void Cameras::evt_SelectionChanged(wxCommandEvent& evt)
{
	this->DoWriteToFileEvent(this->GetPaneHost()->GetFileManager()->GetData());

	int index = this->m_lb_cameras->GetSelection();
	if (index != -1)
	{
		nlohmann::json& data = this->GetPaneHost()->GetFileManager()->GetData();

		std::string key = this->m_camera_names.at(index);

		this->m_txt_name->SetValue(key);

		this->m_vct_pos->SetValues({
			data["cameras"][key]["position"][0].get<double>(),
			data["cameras"][key]["position"][1].get<double>(),
			data["cameras"][key]["position"][2].get<double>()
			});

		this->m_vct_rot->SetValues({
			data["cameras"][key]["rotation"][0].get<double>(),
			data["cameras"][key]["rotation"][1].get<double>(),
			data["cameras"][key]["rotation"][2].get<double>()
			});

		this->m_vct_clips->SetValues({
			data["cameras"][key]["clips"][0].get<double>(),
			data["cameras"][key]["clips"][1].get<double>()
			});

		this->m_spndbl_fov->SetValue(data["cameras"][key]["fov"].get<double>());
	}

	this->m_prev_selection_index = index;

	evt.Skip();
}

void Cameras::evt_NameChanged(wxCommandEvent& evt)
{
	int index = this->m_lb_cameras->GetSelection();
	if (index != -1)
	{
		this->DoWriteToFileEvent(this->GetPaneHost()->GetFileManager()->GetData());

		if (this->m_camera_names.at(index) != this->m_txt_name->GetValue())
		{
			this->m_camera_names.at(index) = this->m_txt_name->GetValue();

			this->m_lb_cameras->Delete(index);
			this->m_lb_cameras->Insert(this->m_txt_name->GetValue(), index);
			this->m_lb_cameras->SetSelection(index);
		}
	}

	evt.Skip();
}

void Cameras::evt_FOVChanged(wxSpinDoubleEvent& evt)
{
	this->DoWriteToFileEvent(this->GetPaneHost()->GetFileManager()->GetData());
	evt.Skip();
}

void Cameras::evt_PosChanged(VectorCtrlEvent& evt)
{
	this->DoWriteToFileEvent(this->GetPaneHost()->GetFileManager()->GetData());
	evt.Skip();
}

void Cameras::evt_RotChanged(VectorCtrlEvent& evt)
{
	this->DoWriteToFileEvent(this->GetPaneHost()->GetFileManager()->GetData());
	evt.Skip();
}

void Cameras::evt_ClipsChanged(VectorCtrlEvent& evt)
{
	this->DoWriteToFileEvent(this->GetPaneHost()->GetFileManager()->GetData());
	evt.Skip();
}

Cameras::Cameras(PaneHost* parent) : Pane(parent)
{
	this->m_sizer = new wxGridBagSizer(0, 0);

	//create UI components
	// list of camera names
	this->m_lb_cameras = new wxListBox(this, wxID_ANY);
	this->m_lb_cameras->Bind(wxEVT_LISTBOX, &Cameras::evt_SelectionChanged, this);
	this->m_sizer->Add(this->m_lb_cameras, wxGBPosition(0, 0), wxGBSpan(1, 2), wxEXPAND | wxALL);

	// edit camera name
	this->m_txt_name = new wxTextCtrl(this, wxID_ANY);
	this->m_txt_name->Bind(wxEVT_TEXT, &Cameras::evt_NameChanged, this);
	this->m_sizer->Add(this->m_txt_name, wxGBPosition(1, 0), wxGBSpan(1, 2), wxEXPAND | wxALL);

	// edit position
	this->m_stxt_pos = new wxStaticText(this, wxID_ANY, "Position");
	this->m_sizer->Add(this->m_stxt_pos, wxGBPosition(2, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_vct_pos = new VectorCtrl(this, wxID_ANY);
	this->m_vct_pos->Bind(EVT_VCTRCTRL_CHANGED, &Cameras::evt_PosChanged, this);
	this->m_sizer->Add(this->m_vct_pos, wxGBPosition(2, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);

	// edit rotation
	this->m_stxt_rot = new wxStaticText(this, wxID_ANY, "Rotation");
	this->m_vct_pos->SetRange(-1000.0, 1000.0);
	this->m_sizer->Add(this->m_stxt_rot, wxGBPosition(3, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_vct_rot = new VectorCtrl(this, wxID_ANY);
	this->m_vct_rot->Bind(EVT_VCTRCTRL_CHANGED, &Cameras::evt_RotChanged, this);
	this->m_sizer->Add(this->m_vct_rot, wxGBPosition(3, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);

	// edit fov
	this->m_stxt_fov = new wxStaticText(this, wxID_ANY, "FOV");
	this->m_sizer->Add(this->m_stxt_fov, wxGBPosition(4, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_spndbl_fov = new wxSpinCtrlDouble(this, wxID_ANY);
	this->m_spndbl_fov->SetValue(0);
	this->m_spndbl_fov->Bind(wxEVT_SPINCTRLDOUBLE, &Cameras::evt_FOVChanged, this);
	this->m_sizer->Add(this->m_spndbl_fov, wxGBPosition(4, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);

	// edit clips
	this->m_stxt_clips = new wxStaticText(this, wxID_ANY, "Clips");
	this->m_sizer->Add(this->m_stxt_clips, wxGBPosition(5, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	{
		VectorCtrlConfig config;
		config.min = 0;
		config.num_fields = 2;
		config.can_be_min = false;

		this->m_vct_clips = new VectorCtrl(this, wxID_ANY, config);
		this->m_vct_clips->Bind(EVT_VCTRCTRL_CHANGED, &Cameras::evt_ClipsChanged, this);
		this->m_sizer->Add(this->m_vct_clips, wxGBPosition(5, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);
	}

	this->m_sizer->AddGrowableCol(1);
	this->m_sizer->AddGrowableRow(0);

	this->SetSizer(this->m_sizer);
	this->Centre(wxBOTH);
	this->Show();
}

std::string Cameras::GetDisplayName()
{
	return "Cameras";
}

void Cameras::SceneChangedEvent(Scene* scene)
{
	this->m_lb_cameras->Clear();
	this->m_camera_names.clear();
	for (const auto& camera: this->GetPaneHost()->GetFileManager()->GetData()["cameras"].items())
	{
		this->m_lb_cameras->Append(camera.key());
		this->m_camera_names.push_back(camera.key());
	}
}

void Cameras::DoWriteToFileEvent(nlohmann::json& data)
{
	if (this->m_prev_selection_index != -1)
	{
		std::string key = this->m_camera_names.at(this->m_prev_selection_index);

		data["cameras"][key]["fov"] = this->m_spndbl_fov->GetValue();

		{
			std::vector<double> values = this->m_vct_pos->GetValues();
			data["cameras"][key]["position"] = nlohmann::json::array({ values.at(0), values.at(1), values.at(2) });
		}

		{
			std::vector<double> values = this->m_vct_rot->GetValues();
			data["cameras"][key]["rotation"] = nlohmann::json::array({ values.at(0), values.at(1), values.at(2) });
		}

		{
			std::vector<double> values = this->m_vct_clips->GetValues();
			data["cameras"][key]["clips"] = nlohmann::json::array({ values.at(0), values.at(1) });
		}

		if (key != this->m_txt_name->GetValue())
		{
			nlohmann::json& config = this->GetPaneHost()->GetFileManager()->GetData();
			config["cameras"][this->m_txt_name->GetValue()] = config["cameras"][key];
			config["cameras"].erase(key);
		}
	}
}
