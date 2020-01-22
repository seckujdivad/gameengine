#pragma once

#include <wx/glcanvas.h>
#include <wx/dcclient.h>

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "../Scene.h"

class Scene;

class EngineCanvas : public wxGLCanvas
{
private:
	wxGLContext* m_glcontext;
	Scene* m_scene;

	unsigned int m_VAO;
	unsigned int m_shader_program;

	glm::mat4 m_perspmat;
	glm::mat4 m_viewmat;
	glm::mat4 m_mdlmat;

	GLuint m_perspmat_id;
	GLuint m_viewmat_id;
	GLuint m_mdlmat_id;

public:
	EngineCanvas(wxWindow* parent, wxWindowID id, const int* args);
	~EngineCanvas();

	void SetScene(Scene* scene);
	void Render();

	void Paint(wxPaintEvent& evt);
};