#include <wx/wxprec.h>
#include "Viewport.h"

Viewport::Viewport(PaneHost* parent) : Pane(parent)
{
	this->SetMinSize(wxSize(100, 100));

	this->m_sizer = new wxGridBagSizer(0, 0);
	this->m_sizer->SetFlexibleDirection(wxBOTH);
	this->m_sizer->SetNonFlexibleGrowMode(wxFLEX_GROWMODE_SPECIFIED);

	wxGLAttributes args;
	args.PlatformDefaults().Depth(24).Stencil(8).RGBA().DoubleBuffer().EndList();

	this->m_glcanvas = new EngineCanvas(this, wxID_ANY, args);
	this->m_glcanvas->SetRenderLoop(false);
	this->m_sizer->Add(this->m_glcanvas, wxGBPosition(0, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_glcanvas->SetMinSize(wxSize(100, 100));

	this->m_glcanvas->SetPostProcessorShaderProgram(new ShaderProgram(
		{
			{ "resources/shaders/postprocess.vert", GL_VERTEX_SHADER },
			{ "resources/shaders/postprocess.frag", GL_FRAGMENT_SHADER }
		},
		{}));

	this->SetSizer(this->m_sizer);
	this->Centre(wxBOTH);
	this->Show();
}

std::string Viewport::GetDisplayName()
{
	return "Viewport";
}

void Viewport::SceneChangedEvent(Scene* scene)
{
	this->m_glcanvas->SetScene(scene);
	this->m_glcanvas->SetActiveCamera(scene->cameras.at(0));

	this->m_glcanvas->SetMouselook(true);
	this->m_glcanvas->SetKeyboardMove(true);
	this->m_glcanvas->SetRenderLoop(true);

	this->m_glcanvas->SetSize(this->GetSize());
}
