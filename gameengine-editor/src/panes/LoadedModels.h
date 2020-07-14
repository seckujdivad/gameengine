#pragma once

#include <wx/wx.h>
#include <wx/sizer.h>
#include <wx/listbox.h>

#include <nlohmann/json.hpp>

#include "Pane.h"
#include "../PaneHost.h"

class LoadedModels : public Pane
{
private:
	wxGridBagSizer* m_sizer;

	wxListBox* m_lb_models;

public:
	LoadedModels(PaneHost* parent);
	~LoadedModels();

	std::string GetDisplayName() override;

	void SceneChangedEvent(Scene* scene) override;
	void DoWriteToFileEvent(nlohmann::json& data) override;
};