#include <wx/wxprec.h>
#include "Viewport.h"

void Viewport::Resized(wxSizeEvent& evt)
{
	this->m_glcanvas->SetMinSize(this->GetSize());
	evt.Skip();
}

Viewport::Viewport(PaneHost* parent) : Pane(parent)
{
	//this->SetMinSize(wxSize(100, 100));

	this->m_sizer = new wxGridBagSizer(10, 10);
	this->m_sizer->SetFlexibleDirection(wxBOTH);
	this->m_sizer->SetNonFlexibleGrowMode(wxFLEX_GROWMODE_SPECIFIED);

	this->m_glcanvas = this->GetPaneHost()->GetEngine()->GenerateNewCanvas(wxID_ANY, this);
	this->m_glcanvas->MakeOpenGLFocus();
	this->m_glcanvas->SetRenderLoop(false);
	this->m_sizer->Add(this->m_glcanvas, wxGBPosition(0, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_glcanvas->SetPostProcessorShaderProgram(new ShaderProgram(
		{
			{ GetEmbeddedTextfile(RCID_TF_POSTPROCESS_VERTSHADER), GL_VERTEX_SHADER },
			{ GetEmbeddedTextfile(RCID_TF_POSTPROCESS_FRAGSHADER), GL_FRAGMENT_SHADER }
		},
		{},
		false));

	this->Bind(wxEVT_SIZE, &Viewport::Resized, this);

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
	this->m_glcanvas->MakeOpenGLFocus();
	this->m_glcanvas->SetScene(scene);
	this->m_glcanvas->SetActiveCamera(scene->cameras.at(0));

	this->m_glcanvas->SetMouselook(true);
	this->m_glcanvas->SetKeyboardMove(true);
	this->m_glcanvas->SetRenderLoop(true);
}
