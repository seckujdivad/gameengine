#include "LoadedModels.h"

LoadedModels::LoadedModels(PaneHost* parent) : Pane(parent)
{
	this->m_sizer = new wxGridBagSizer(0, 0);

	this->SetSizer(this->m_sizer);
	this->Centre(wxBOTH);
	this->Show();
}

LoadedModels::~LoadedModels()
{
}

std::string LoadedModels::GetDisplayName()
{
	return "Loaded Models";
}

void LoadedModels::SceneChangedEvent(Scene* scene)
{
}

void LoadedModels::DoWriteToFileEvent(nlohmann::json& data)
{
}
