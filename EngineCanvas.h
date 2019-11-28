#pragma once

#include <wx/glcanvas.h>

class EngineCanvas : public wxGLCanvas
{
private:
public:
	EngineCanvas(wxWindow* parent, wxWindowID id = wxID_ANY);

	void Paint(wxPaintEvent& evt);
};