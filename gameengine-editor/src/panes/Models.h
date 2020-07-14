#pragma once

#include <wx/wx.h>
#include <wx/gbsizer.h>
#include <wx/listbox.h>
#include <wx/button.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include <nlohmann/json.hpp>

#include "Pane.h"
#include "../PaneHost.h"
#include "../VectorCtrl.h"

class Models : public Pane
{
private:
	wxGridBagSizer* m_sizer;

	wxListBox* m_lb_models;
	wxButton* m_btn_mdl_new;
	wxTextCtrl* m_txt_mdl_name;

	void event_lb_models_clicked(wxCommandEvent& evt);
	void event_txt_mdl_name_updated(wxCommandEvent& evt);

	void UpdateModelName(Model* model, std::string name);
	void ModelSelectionUpdated(Model* new_model, Model* old_model);

public:
	Models(PaneHost* parent);
	~Models();

	std::string GetDisplayName() override;

	void SceneChangedEvent(Scene* scene) override;
	void DoWriteToFileEvent(nlohmann::json& data) override;
};