#include <wx/wxprec.h>
#include "Models.h"

Models::Models(PaneHost* parent) : Pane(parent)
{
	this->m_sizer = new wxGridBagSizer(0, 0);

	this->m_lb_models = new wxListBox(this, wxID_ANY);
	this->m_sizer->Add(this->m_lb_models, wxGBPosition(0, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_sizer->AddGrowableCol(0);
	this->m_sizer->AddGrowableRow(0);

	if (this->GetPaneHost()->GetScene() != nullptr)
	{
		this->SceneChangedEvent(this->GetPaneHost()->GetScene());
	}

	this->SetSizer(this->m_sizer);
	this->Centre(wxBOTH);
	this->Show();
}

Models::~Models()
{
}

std::string Models::GetDisplayName()
{
	return "Models";
}

void Models::SceneChangedEvent(Scene* scene)
{
	std::vector<wxString> names;
	for (size_t i = 0; i < scene->models.size(); i++)
	{
		names.push_back(scene->models.at(i)->GetIdentifier());
	}
	
	this->m_lb_models->Clear();
	this->m_lb_models->Insert(names, 0);
}
