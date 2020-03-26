#pragma once

#include "../GLComponents.h"
#include <wx/dcclient.h>

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <iostream>
#include <fstream>

#include "../scene/Scene.h"

#ifndef ENGINECANVAS_LOG_PATH
#define ENGINECANVAS_LOG_PATH "gameengine_GL.log"
#endif

class Scene;

class EngineCanvas : public wxGLCanvas
{
private:
	wxGLContext* m_glcontext;
	Scene* m_scene = nullptr;

	unsigned int m_VAO;
	unsigned int m_shader_program;

	glm::mat4 m_perspmat;
	glm::mat4 m_viewmat;
	glm::mat4 m_mdlmat;

	GLuint m_perspmat_id;
	GLuint m_viewmat_id;
	GLuint m_mdlmat_id;

public:
	EngineCanvas(wxWindow* parent, wxWindowID id, wxGLAttributes& args);
	~EngineCanvas();

	void SetScene(Scene* scene);
	void Render();

	void Paint(wxPaintEvent& evt);
};

void GLAPIENTRY MessageCallback(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar* message, const void* userParam);