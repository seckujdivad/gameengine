#include <wx/wxprec.h>
#include "Lights.h"

Lights::Lights(PaneHost* parent) : Pane(parent)
{
	this->m_sizer = new wxGridBagSizer(0, 0);

	this->SetSizer(this->m_sizer);
	this->Centre(wxBOTH);
	this->Show();
}

Lights::~Lights()
{
}

std::string Lights::GetDisplayName()
{
	return "Lights";
}
