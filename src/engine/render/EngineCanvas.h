#pragma once

#include <wx/glcanvas.h>
#include <wx/dcclient.h>

#include "../Scene.h"

class Scene;

class EngineCanvas : public wxGLCanvas
{
private:
	wxGLContext* m_glcontext;
	Scene* m_scene;

public:
	EngineCanvas(wxWindow* parent, wxWindowID id = -1);
	~EngineCanvas();

	void SetScene(Scene* scene);
	void Render();

	void Paint(wxPaintEvent& evt);
};