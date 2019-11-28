#include "EngineCanvas.h"

EngineCanvas::EngineCanvas(wxWindow* parent, wxWindowID id = wxID_ANY) : wxGLCanvas(parent, id)
{
	this->Bind(wxEVT_PAINT, &this->Paint, this, wxID_ANY);
}

void EngineCanvas::Paint(wxPaintEvent& evt)
{

}