#pragma once

#include <wx/glcanvas.h>
#include <wx/dcclient.h>

#include <glm/glm.hpp>

#include "../Scene.h"

class Scene;

class EngineCanvas : public wxGLCanvas
{
private:
	wxGLContext* m_glcontext;
	Scene* m_scene;

public:
	unsigned int m_VAO;
	unsigned int m_shader_program;

	EngineCanvas(wxWindow* parent, wxWindowID id, const int* args);
	~EngineCanvas();

	void SetScene(Scene* scene);
	void Render();

	void Paint(wxPaintEvent& evt);
};