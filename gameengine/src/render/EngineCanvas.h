#pragma once

#include "../GLComponents.h"

#include <wx/wx.h>
#include <wx/dcclient.h>

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
	float m_mouselook_multiplier = 3.0f;

	//keyboard move
	bool m_keyboard_move = false;
	Camera* m_move_camera = nullptr;
	wxTimer* m_timer_mainloop;
	float m_keyboard_move_increment = 0.5f;

	void CameraControlMainloop(wxTimerEvent& evt);
	void RenderMainloop(wxIdleEvent& evt);

public:
	EngineCanvas(wxWindow* parent, wxWindowID id, wxGLAttributes& args);
	~EngineCanvas();

	void SetScene(Scene* scene);
	void Render();

	void SetMouselook(bool enable, Camera* camera = nullptr);
	void SetKeyboardMove(bool enable, Camera* camera = nullptr);
	void SetRenderLoop(bool enable);
};

void GLAPIENTRY MessageCallback(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar* message, const void* userParam);