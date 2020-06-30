#include "Cameras.h"

void Cameras::evt_SelectionChanged(wxCommandEvent& evt)
{
	int index = this->m_lb_cameras->GetSelection();
	if (index != -1)
	{
		std::string key = this->m_camera_names.at(index);
		nlohmann::json data = this->GetPaneHost()->GetFileManager()->GetData()["cameras"][key];
		
		this->m_txt_name->SetValue(key);
		this->m_vct_pos->SetValues({
			data["position"][0].get<double>(),
			data["position"][1].get<double>(),
			data["position"][2].get<double>()
			});
		this->m_vct_rot->SetValues({
			data["rotation"][0].get<double>(),
			data["rotation"][1].get<double>(),
			data["rotation"][2].get<double>()
			});
		this->m_vct_clips->SetValues({
			data["clips"][0].get<double>(),
			data["clips"][1].get<double>()
			});
		this->m_spndbl_fov->SetValue(data["fov"].get<double>());
	}

	evt.Skip();
}

void Cameras::evt_NameChanged(wxCommandEvent& evt)
{
	evt.Skip();
}

void Cameras::evt_FOVChanged(wxSpinDoubleEvent& evt)
{
	evt.Skip();
}

void Cameras::evt_PosChanged(VectorCtrlEvent& evt)
{
	evt.Skip();
}

void Cameras::evt_RotChanged(VectorCtrlEvent& evt)
{
	evt.Skip();
}

void Cameras::evt_ClipsChanged(VectorCtrlEvent& evt)
{
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

	this->m_vct_pos = new VectorCtrl(this, wxID_ANY, 3);
	this->m_vct_pos->Bind(EVT_VCTRCTRL_CHANGED, &Cameras::evt_PosChanged, this);
	this->m_sizer->Add(this->m_vct_pos, wxGBPosition(2, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);

	// edit rotation
	this->m_stxt_rot = new wxStaticText(this, wxID_ANY, "Rotation");
	this->m_sizer->Add(this->m_stxt_rot, wxGBPosition(3, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_vct_rot = new VectorCtrl(this, wxID_ANY, 3);
	this->m_vct_rot->Bind(EVT_VCTRCTRL_CHANGED, &Cameras::evt_RotChanged, this);
	this->m_sizer->Add(this->m_vct_rot, wxGBPosition(3, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);

	// edit fov
	this->m_stxt_fov = new wxStaticText(this, wxID_ANY, "FOV");
	this->m_sizer->Add(this->m_stxt_fov, wxGBPosition(4, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_spndbl_fov = new wxSpinCtrlDouble(this, wxID_ANY);
	this->m_spndbl_fov->SetValue(0);
	this->m_sizer->Add(this->m_spndbl_fov, wxGBPosition(4, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);

	// edit clips
	this->m_stxt_clips = new wxStaticText(this, wxID_ANY, "Clips");
	this->m_sizer->Add(this->m_stxt_clips, wxGBPosition(5, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_vct_clips = new VectorCtrl(this, wxID_ANY, 2);
	this->m_vct_clips->Bind(EVT_VCTRCTRL_CHANGED, &Cameras::evt_ClipsChanged, this);
	this->m_sizer->Add(this->m_vct_clips, wxGBPosition(5, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);

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
