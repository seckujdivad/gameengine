#include <wx/wxprec.h>
#include "Models.h"

void Models::event_lb_models_clicked(wxCommandEvent& evt)
{
	int selection_index = this->m_lb_models->GetSelection();
	if (selection_index != wxNOT_FOUND)
	{
		this->GetPaneHost()->SetSelectedModel(selection_index);
		this->m_txt_mdl_name->SetValue(this->GetPaneHost()->GetSelectedModel()->GetIdentifier());
	}
	evt.Skip();
}

void Models::event_txt_mdl_name_updated(wxCommandEvent& evt)
{
	if (this->GetPaneHost()->GetSelectedModel() != nullptr)
	{
		std::string new_mdl_name = this->m_txt_mdl_name->GetValue();

		FileChange change;
		change.data = new_mdl_name;
		change.key_path = { "layout", this->GetPaneHost()->GetSelectedModel()->GetIdentifier() };
		change.mode = FC_MODE_RENAME;
		this->GetPaneHost()->GetFileManager()->QueueChange(change);

		this->GetPaneHost()->GetSelectedModel()->SetIdentifier(new_mdl_name);

		int selection_index = this->m_lb_models->GetSelection();
		this->m_lb_models->Delete(selection_index);
		this->m_lb_models->Insert(new_mdl_name, selection_index);
		this->m_lb_models->SetSelection(selection_index);
	}
	evt.Skip();
}

Models::Models(PaneHost* parent) : Pane(parent)
{
	this->m_sizer = new wxGridBagSizer(0, 0);

	this->m_lb_models = new wxListBox(this, wxID_ANY);
	this->m_lb_models->Bind(wxEVT_LISTBOX, &Models::event_lb_models_clicked, this);
	this->m_sizer->Add(this->m_lb_models, wxGBPosition(0, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_btn_mdl_new = new wxButton(this, wxID_ANY, "New");
	this->m_sizer->Add(this->m_btn_mdl_new, wxGBPosition(2, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_txt_mdl_name = new wxTextCtrl(this, wxID_ANY);
	this->m_txt_mdl_name->Bind(wxEVT_TEXT, &Models::event_txt_mdl_name_updated, this);
	this->m_sizer->Add(this->m_txt_mdl_name, wxGBPosition(1, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_sizer->AddGrowableCol(0);
	this->m_sizer->AddGrowableRow(0);

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
