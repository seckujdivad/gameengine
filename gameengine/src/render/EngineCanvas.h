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
	bool m_mouselook_active = false;
	Camera* m_look_camera = nullptr;
	wxCursor m_blank_cursor;
	float m_mouselook_multiplier = 3.0f;

	//keyboard move
	bool m_keyboard_move = false;
	Camera* m_move_camera = nullptr;
	wxTimer* m_timer_mainloop;
	float m_keyboard_move_increment = 0.1f;

	void CameraControlMainloop(wxTimerEvent& evt);

	//render loop
	bool m_loop_render = false;
	void RenderMainloop(wxIdleEvent& evt);

	//key press handling
	void KeyDown(wxKeyEvent& evt);
	void Clicked(wxMouseEvent& evt);
	void SetMouselookActive(bool enable);

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