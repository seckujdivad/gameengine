#pragma once

#include "../../../GLComponents.h"

#include <wx/wx.h>
#include <wx/dcclient.h>

#include "../target/RenderTarget.h"

class EngineCanvas : public wxGLCanvas, public RenderTarget
{
private:
	//rendering
	wxGLContext* m_glcontext;

	void OnPaint(wxPaintEvent& evt);
	void OnSize(wxSizeEvent& evt);

	//mouse look
	Camera* m_camera_controlled = nullptr;
	bool m_mouselook = false;
	bool m_mouselook_active = false;
	wxCursor m_blank_cursor;
	float m_mouselook_multiplier = 3.0f;

	void SetMouselookActive(bool enable);

	//keyboard move
	bool m_keyboard_move = false;
	bool m_keyboard_move_active = false;
	wxTimer* m_timer_mainloop;
	double m_keyboard_move_increment = 0.1;

	void SetKeyboardMoveActive(bool enable);
	void CameraControlMainloop(wxTimerEvent& evt);

	//render loop
	bool m_loop_render = false;
	void RenderMainloop(wxIdleEvent& evt);

	//key press handling
	void KeyDown(wxKeyEvent& evt);
	void Clicked(wxMouseEvent& evt);

	void PreRenderEvent() override;
	void PostRenderEvent() override;

public:
	EngineCanvas(wxWindow* parent, wxWindowID id, wxGLAttributes& args, wxGLContext* context, Engine* engine, RenderTargetConfig config);
	EngineCanvas(const EngineCanvas&) = delete;
	EngineCanvas& operator=(const EngineCanvas&) = delete;
	EngineCanvas(EngineCanvas&&) = delete;
	EngineCanvas& operator=(EngineCanvas&&) = delete;
	~EngineCanvas();

	void SetMouselook(bool enable);
	void SetKeyboardMove(bool enable);
	void SetRenderLoop(bool enable);

	void SetVerticalSync(bool enabled);

	std::tuple<int, int> GetOutputSize() const override;
	bool SetOutputSize(std::tuple<int, int> dimensions) override;

	void MakeOpenGLFocus();

	void SetControlledCamera(Camera* camera);
	Camera* GetControlledCamera() const;

	bool SwapBuffers() override;
};