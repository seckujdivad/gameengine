#pragma once

#include <wx/glcanvas.h>
#include <wx/dcclient.h>

#include <gl/GL.h>

class EngineCanvas : public wxGLCanvas
{
private:
	wxGLContext* m_glcontext;

public:
	EngineCanvas(wxWindow* parent, wxWindowID id = -1);
	~EngineCanvas();

	void Paint(wxPaintEvent& evt);

	void Render();
};