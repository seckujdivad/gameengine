#include <wx/wxprec.h>
#include "Main.h"


Main::Main() : wxFrame(nullptr, wxID_ANY, "Render test", wxPoint(30, 30), wxSize(800, 600))
{
	this->SetBackgroundColour(wxColor(238, 238, 238));
	this->SetMinSize(wxSize(500, 400));

	//load scene
	this->m_scene = InitialiseScene("resources", "simplescene.json");

	this->m_sizer = new wxGridBagSizer(0, 0);
	this->m_sizer->SetFlexibleDirection(wxBOTH);
	this->m_sizer->SetNonFlexibleGrowMode(wxFLEX_GROWMODE_SPECIFIED);

	int gl_args[] = {
		WX_GL_CORE_PROFILE,
		WX_GL_MAJOR_VERSION, 4,
		WX_GL_MINOR_VERSION, 3,
		0
	};

	this->m_glcanvas = new EngineCanvas(this, wxID_ANY, gl_args);
	this->m_sizer->Add(this->m_glcanvas, wxGBPosition(0, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_btn_render = new wxButton(this, wxID_ANY, wxString("Render"));
	this->m_btn_render->Bind(wxEVT_BUTTON, &Main::btn_render_OnClick, this);
	this->m_sizer->Add(this->m_btn_render, wxGBPosition(1, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_sizer->AddGrowableRow(0);
	this->m_sizer->AddGrowableCol(0);

	this->SetSizer(this->m_sizer);
	this->Centre(wxBOTH);
	this->Layout();
}

Main::~Main()
{
	delete this->m_scene;
}

void Main::btn_render_OnClick(wxCommandEvent& evt)
{
	//this->m_glcanvas->Render();
	this->m_scene->Render(this->m_glcanvas);
	evt.Skip();
}