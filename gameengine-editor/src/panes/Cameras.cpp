#include "Cameras.h"

void Cameras::evt_PosChanged(VectorCtrlEvent& evt)
{
	evt.Skip();
}

Cameras::Cameras(PaneHost* parent) : Pane(parent)
{
	this->m_sizer = new wxGridBagSizer(0, 0);

	this->m_lb_cameras = new wxListBox(this, wxID_ANY);
	this->m_sizer->Add(this->m_lb_cameras, wxGBPosition(0, 0), wxGBSpan(1, 2), wxEXPAND | wxALL);

	this->m_stxt_fov = new wxStaticText(this, wxID_ANY, "FOV");
	this->m_sizer->Add(this->m_stxt_fov, wxGBPosition(1, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_spndbl_fov = new wxSpinCtrlDouble(this, wxID_ANY);
	this->m_spndbl_fov->SetValue(0);
	this->m_sizer->Add(this->m_spndbl_fov, wxGBPosition(1, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_stxt_pos = new wxStaticText(this, wxID_ANY, "Position");
	this->m_sizer->Add(this->m_stxt_pos, wxGBPosition(2, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_vct_pos = new VectorCtrl(this, wxID_ANY, 3);
	this->m_vct_pos->Bind(EVT_VCTRCTRL_CHANGED, &Cameras::evt_PosChanged, this);
	this->m_sizer->Add(this->m_vct_pos, wxGBPosition(2, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);

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
	for (std::vector<Camera*>::iterator it = scene->cameras.begin(); it != scene->cameras.end(); it++)
	{
		this->m_lb_cameras->Append((*it)->GetIdentifier());
	}
}
