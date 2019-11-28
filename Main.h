#pragma once

#include <wx/wx.h>
#include <wx/glcanvas.h>
#include <wx/gbsizer.h>

#include "EngineCanvas.h"


class Main : public wxFrame
{
private:
	wxGridBagSizer* m_sizer;
	wxGLCanvas* m_glcanvas;

public:
	Main();
	~Main();
};