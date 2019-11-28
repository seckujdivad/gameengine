#include "Main.h"


Main::Main() : wxFrame(nullptr, wxID_ANY, "Render test", wxPoint(30, 30), wxSize(800, 600))
{
	this->SetBackgroundColour("#EEEEEE");

	this->m_sizer = new wxGridBagSizer(0, 0);
	this->m_sizer->SetFlexibleDirection(wxBOTH);
	this->m_sizer->SetNonFlexibleGrowMode(wxFLEX_GROWMODE_SPECIFIED);

	this->m_glcanvas = new wxGLCanvas(this, wxID_ANY);

	this->m_sizer->Add(this->m_glcanvas, wxGBPosition(0, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_sizer->AddGrowableRow(0);
	this->m_sizer->AddGrowableCol(0);

	this->SetSizer(this->m_sizer);
	this->Centre(wxBOTH);
	this->Layout();
}

Main::~Main()
{

}