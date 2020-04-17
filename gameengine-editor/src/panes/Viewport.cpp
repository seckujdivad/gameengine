#include <wx/wxprec.h>
#include "Viewport.h"

Viewport::Viewport(PaneHost* parent) : Pane(parent)
{
	//this->SetMinSize(wxSize(100, 100));

	this->m_sizer = new wxBoxSizer(wxHORIZONTAL);

	wxButton* button = new wxButton(this, wxID_ANY, "Hello world");
	this->m_sizer->Add(button);

	this->SetSizer(this->m_sizer);
	this->Centre(wxBOTH);
	this->Show();
}

std::string Viewport::GetDisplayName()
{
	return "Viewport";
}
