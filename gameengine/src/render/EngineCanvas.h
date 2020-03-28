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

class Camera;

class EngineCanvas : public wxGLCanvas
{
private:
	//rendering
	wxGLContext* m_glcontext;
	Scene* m_scene = nullptr;

	void Paint(wxPaintEvent& evt);

	//mouse look
	bool m_mouselook = false;
	Camera* m_look_camera = nullptr;
	wxCursor m_blank_cursor;
	
	void MouseMove(wxMouseEvent& evt);

public:
	EngineCanvas(wxWindow* parent, wxWindowID id, wxGLAttributes& args);
	~EngineCanvas();

	void SetScene(Scene* scene);
	void Render();

	void SetMouselook(bool enable, Camera* camera = nullptr);
};

void GLAPIENTRY MessageCallback(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar* message, const void* userParam);