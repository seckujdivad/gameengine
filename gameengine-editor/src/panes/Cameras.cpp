#include "Cameras.h"

Cameras::Cameras(PaneHost* parent) : Pane(parent)
{
	this->m_sizer = new wxGridBagSizer(0, 0);

	this->m_lb_cameras = new wxListBox(this, wxID_ANY);
	this->m_sizer->Add(this->m_lb_cameras, wxGBPosition(0, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_sizer->AddGrowableCol(0);
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
