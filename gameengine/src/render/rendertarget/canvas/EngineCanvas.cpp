#include "EngineCanvas.h"

#if defined(GAMEENGINE_USE_WGL)
#include <GL/wglew.h>
#elif defined(GAMEENGINE_USE_GLX)
#include <GL/glxew.h>
#else
#error
#endif

#include "../../../Engine.h"
#include "../../../scene/Camera.h"
#include "../target/RenderTargetMode.h"

EngineCanvas::EngineCanvas(wxWindow* parent, wxWindowID id, wxGLAttributes& args, wxGLContext* context, Engine* engine, RenderTargetConfig config)
	: wxGLCanvas(parent, args, id),
	RenderTarget(engine, config),
	m_glcontext(context)
{
	this->SetFramebuffer(GLFramebuffer(0, false));

	this->m_blank_cursor = wxCursor(wxCURSOR_BLANK);
	
	this->m_timer_mainloop = new wxTimer(this);
	this->Bind(wxEVT_TIMER, &EngineCanvas::CameraControlMainloop, this);
	this->m_timer_mainloop->Start(10);

	this->Bind(wxEVT_PAINT, &EngineCanvas::OnPaint, this);
	this->Bind(wxEVT_SIZE, &EngineCanvas::OnSize, this);
	this->Bind(wxEVT_KEY_DOWN, &EngineCanvas::KeyDown, this);
	this->Bind(wxEVT_LEFT_DOWN, &EngineCanvas::Clicked, this);
}

EngineCanvas::~EngineCanvas()
{
	this->m_timer_mainloop->Stop();
	delete this->m_timer_mainloop;
}

void EngineCanvas::OnPaint(wxPaintEvent& evt)
{
	this->Draw(false);
	evt.Skip();
}

void EngineCanvas::OnSize(wxSizeEvent& evt)
{
	this->Update();
	evt.Skip();
}

void EngineCanvas::CameraControlMainloop(wxTimerEvent& evt)
{
	//stop timer
	this->m_timer_mainloop->Stop();

	//make sure the control still has focus
	if ((this->m_keyboard_move_active || this->m_mouselook_active) && !this->HasFocus())
	{
		this->SetMouselookActive(false);
		this->SetKeyboardMoveActive(false);
	}

	if (this->GetControlledCamera() != nullptr)
	{
		//get mouse info
		wxMouseState mouse_state = wxGetMouseState();
		int screen_centre[2] = { this->GetSize().GetX() / 2, this->GetSize().GetY() / 2 };
		int mouse_position[2] = { mouse_state.GetPosition().x - this->GetScreenPosition().x, mouse_state.GetPosition().y - this->GetScreenPosition().y };

		//check mouse delta and apply
		if (this->m_mouselook_active)
		{
			int mousedelta[2] = { mouse_position[0] - screen_centre[0], mouse_position[1] - screen_centre[1] };

			float fov_fraction_x = (static_cast<float>(mousedelta[0]) * this->m_mouselook_multiplier) / static_cast<float>(this->GetSize().GetX());
			float fov_fraction_y = (static_cast<float>(mousedelta[1]) * this->m_mouselook_multiplier) / static_cast<float>(this->GetSize().GetX());

			float fov = static_cast<float>(this->GetControlledCamera()->GetFOV());

			float rotation_z = fov_fraction_x * fov;
			float rotation_x = fov_fraction_y * fov;

			this->GetControlledCamera()->SetRotation(0, this->GetControlledCamera()->GetRotation(0) - rotation_x);
			this->GetControlledCamera()->SetRotation(2, this->GetControlledCamera()->GetRotation(2) - rotation_z);

			this->WarpPointer(screen_centre[0], screen_centre[1]);
		}

		//poll keyboard movement keys
		if (this->m_keyboard_move_active)
		{
			glm::dvec3 local_translation = glm::dvec3(0.0);

			if (wxGetKeyState(wxKeyCode('W')))
			{
				local_translation += glm::dvec3(0.0, 0.0, -1.0);
			}
			if (wxGetKeyState(wxKeyCode('S')))
			{
				local_translation += glm::dvec3(0.0, 0.0, 1.0);
			}
			if (wxGetKeyState(wxKeyCode('D')))
			{
				local_translation += glm::dvec3(1.0, 0.0, 0.0);
			}
			if (wxGetKeyState(wxKeyCode('A')))
			{
				local_translation += glm::dvec3(-1.0, 0.0, 0.0);
			}
			if (wxGetKeyState(WXK_CONTROL))
			{
				local_translation += glm::dvec3(0.0, -1.0, 0.0);
			}
			if (wxGetKeyState(WXK_SHIFT))
			{
				local_translation += glm::dvec3(0.0, 1.0, 0.0);
			}

			//apply movement speed modifiers
			local_translation *= this->m_keyboard_move_increment;
			if (wxGetKeyState(WXK_SPACE))
			{
				local_translation *= 5.0;
			}

			this->GetControlledCamera()->MoveLocally(local_translation, true);
		}
	}

	//restart timer
	this->m_timer_mainloop->Start();
	evt.Skip();
}

void EngineCanvas::KeyDown(wxKeyEvent& evt)
{
	if ((evt.GetKeyCode() == WXK_ESCAPE) && this->m_mouselook_active)
	{
		this->SetMouselookActive(false);
		this->SetKeyboardMoveActive(false);
	}

	evt.Skip();
}

void EngineCanvas::Clicked(wxMouseEvent& evt)
{
	if (this->GetControlledCamera() != nullptr)
	{
		if (!this->m_mouselook_active)
		{
			this->SetMouselookActive(true);
			this->SetKeyboardMoveActive(true);
		}
	}
	
	evt.Skip();
}

void EngineCanvas::PostRenderEvent()
{
	this->SwapBuffers();
}

void EngineCanvas::PreRenderEvent()
{
	this->m_camera_controlled->SetViewportDimensions(std::tuple(this->GetSize().GetX(), this->GetSize().GetY()));
}

void EngineCanvas::SetMouselookActive(bool enable)
{
	if (enable)
	{
		this->SetCursor(this->m_blank_cursor);
		this->WarpPointer(this->GetSize().GetX() / 2, this->GetSize().GetY() / 2);
	}
	else
	{
		this->SetCursor(wxNullCursor);
	}

	this->m_mouselook_active = enable;
}

void EngineCanvas::SetMouselook(bool enable)
{
	if (!(enable && this->m_mouselook_active))
	{
		this->SetMouselookActive(false);
	}
	this->m_mouselook = enable;
}

void EngineCanvas::SetKeyboardMove(bool enable)
{
	this->m_keyboard_move = enable;
	this->SetKeyboardMoveActive(enable);
}

void EngineCanvas::SetKeyboardMoveActive(bool enable)
{
	this->m_keyboard_move_active = enable;
}

void EngineCanvas::SetVerticalSync(bool enabled)
{
#if defined(GAMEENGINE_USE_WGL)
	wglSwapIntervalEXT(enabled);
#elif defined(GAMEENGINE_USE_GLX)
	glXSwapIntervalEXT(enabled); //according to https://www.khronos.org/opengl/wiki/Swap_Interval, untested
#else
#error
#endif
}

std::tuple<int, int> EngineCanvas::GetOutputSize() const
{
	wxSize window_size = this->GetSize();
	return std::tuple(window_size.GetX(), window_size.GetY());
}

bool EngineCanvas::SetOutputSize(std::tuple<int, int> dimensions)
{
	std::tuple<int, int> old_dimensions = this->GetOutputSize();
	if (old_dimensions == dimensions)
	{
		return false;
	}
	else
	{
		this->SetSize(wxSize(std::get<0>(dimensions), std::get<1>(dimensions)));

		return true;
	}
}

void EngineCanvas::MakeOpenGLFocus()
{
	this->SetCurrent(*this->m_glcontext);
}

void EngineCanvas::SetControlledCamera(Camera* camera)
{
	this->m_camera_controlled = camera;
}

Camera* EngineCanvas::GetControlledCamera() const
{
	return this->m_camera_controlled;
}

bool EngineCanvas::SwapBuffers()
{
	return this->wxGLCanvas::SwapBuffers();
}

void EngineCanvas::Draw(bool continuous_draw)
{
	this->GetEngine()->Render(continuous_draw);
}
