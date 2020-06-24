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

		if (new_mdl_name != this->GetPaneHost()->GetSelectedModel()->GetIdentifier())
		{
			nlohmann::json& data = this->GetPaneHost()->GetFileManager()->GetData();
			data["layout"][new_mdl_name] = data["layout"][this->GetPaneHost()->GetSelectedModel()->GetIdentifier()];
			data["layout"].erase(this->GetPaneHost()->GetSelectedModel()->GetIdentifier());

			this->GetPaneHost()->GetSelectedModel()->SetIdentifier(new_mdl_name);

			int selection_index = this->m_lb_models->GetSelection();
			this->m_lb_models->Delete(selection_index);
			this->m_lb_models->Insert(new_mdl_name, selection_index);
			this->m_lb_models->SetSelection(selection_index);
		}
	}
	evt.Skip();
}

void Models::UpdateModelName(Model* model, std::string name)
{
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

	EventSubscription subscription;
	subscription.type = "new mode1 model selected";
	subscription.function = [this](Event evt) { this->ModelSelectionUpdated(
		(Model*)this->GetPaneHost()->GetScene()->GetByIdentifier(evt.data[1].get<std::string>(), 0),
		(Model*)this->GetPaneHost()->GetScene()->GetByIdentifier(evt.data[0].get<std::string>(), 0)); };
	this->GetPaneHost()->GetScene()->GetEventManager()->SubscribeToEvent(subscription);

	subscription.type = "first mode1 model selected";
	subscription.function = [this](Event evt) { this->ModelSelectionUpdated(
		(Model*)this->GetPaneHost()->GetScene()->GetByIdentifier(evt.data[0].get<std::string>(), 0),
		nullptr); };
	this->GetPaneHost()->GetScene()->GetEventManager()->SubscribeToEvent(subscription);
}

void Models::ModelSelectionUpdated(Model* new_model, Model* old_model)
{
	this->UpdateModelName(old_model, this->m_txt_mdl_name->GetValue());
	std::vector<Model*> models = this->GetPaneHost()->GetScene()->models;
	this->m_lb_models->SetSelection(std::distance(models.begin(), std::find(models.begin(), models.end(), new_model)));
	this->m_txt_mdl_name->SetValue(new_model->GetIdentifier());
}
