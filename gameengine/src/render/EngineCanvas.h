#pragma once

#include "../GLComponents.h"

#include <wx/wx.h>
#include <wx/dcclient.h>

#include <iostream>
#include <fstream>

#include "../scene/Scene.h"
#include "ShaderProgram.h"

const char ENGINECANVAS_LOG_PATH[] = "gameengine_GL.log";
const int ENGINECANVAS_NUM_DATA_TEX = 1;

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

	void SetMouselookActive(bool enable);

	//keyboard move
	bool m_keyboard_move = false;
	bool m_keyboard_move_active = false;
	Camera* m_move_camera = nullptr;
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

	//post processing
	int m_old_size[2] = { 1, 1 };

	ShaderProgram* m_postprocessor = nullptr;
	GLuint m_postprocessor_fbo = NULL;
	GLuint m_postprocessor_vao = NULL;
	GLuint m_postprocessor_vbo = NULL;

	GLuint m_postprocessor_depth_texture_write = NULL;
	GLuint m_postprocessor_colour_texture_write = NULL;
	std::vector<GLuint> m_postprocessor_data_textures_write;

	GLuint m_postprocessor_depth_texture_read = NULL;
	GLuint m_postprocessor_colour_texture_read = NULL;
	std::vector<GLuint> m_postprocessor_data_textures_read;

public:
	EngineCanvas(wxWindow* parent, wxWindowID id, wxGLAttributes& args);
	~EngineCanvas();

	void SetScene(Scene* scene);
	void Render();

	void SetMouselook(bool enable, Camera* camera = nullptr);
	void SetKeyboardMove(bool enable, Camera* camera = nullptr);
	void SetRenderLoop(bool enable);

	void SetPostProcessorShaderProgram(ShaderProgram* postprocessor);
};

void GLAPIENTRY MessageCallback(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar* message, const void* userParam);