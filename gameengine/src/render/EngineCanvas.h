#pragma once

#include "../GLComponents.h"

#include <wx/wx.h>
#include <wx/dcclient.h>

#include <iostream>
#include <fstream>

#include <GL/wglew.h>

#include "Renderable.h"

const char ENGINECANVAS_LOG_PATH[] = "gameengine_GL.log";

class EngineCanvas : public wxGLCanvas, public Renderable
{
private:
	//rendering
	wxGLContext* m_glcontext;
	void Paint(wxPaintEvent& evt);

	//mouse look
	bool m_mouselook = false;
	bool m_mouselook_active = false;
	wxCursor m_blank_cursor;
	float m_mouselook_multiplier = 3.0f;

	void SetMouselookActive(bool enable);

	//keyboard move
	bool m_keyboard_move = false;
	bool m_keyboard_move_active = false;
	wxTimer* m_timer_mainloop;
	float m_keyboard_move_increment = 0.1f;

	void SetKeyboardMoveActive(bool enable);
	void CameraControlMainloop(wxTimerEvent& evt);

	//render loop
	bool m_loop_render = false;
	void RenderMainloop(wxIdleEvent& evt);

	//key press handling
	void KeyDown(wxKeyEvent& evt);
	void Clicked(wxMouseEvent& evt);

	void PostRenderEvent();
	void RenderInitialisationEvent();

public:
	EngineCanvas(wxWindow* parent, wxWindowID id, wxGLAttributes& args, wxGLContext* context);
	~EngineCanvas();

	void SetMouselook(bool enable);
	void SetKeyboardMove(bool enable);
	void SetRenderLoop(bool enable);

	void SetVerticalSync(bool enabled);

	std::tuple<int, int> GetOutputSize();

	void MakeOpenGLFocus();
};

void GLAPIENTRY MessageCallback(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar* message, const void* userParam);