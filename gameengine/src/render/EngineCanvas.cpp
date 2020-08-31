#include "EngineCanvas.h"

#include <iostream>
#include <fstream>

#include "../Engine.h"
#include "../scene/Camera.h"

EngineCanvas::EngineCanvas(wxWindow* parent, wxWindowID id, wxGLAttributes& args, wxGLContext* context, Engine* engine, RenderMode mode) : wxGLCanvas(parent, args, id), Renderable(engine, mode)
{
	this->m_glcontext = context;
	this->MakeOpenGLFocus();

	//this->SetVerticalSync(false);

	this->SetFramebuffer(0);

	this->m_blank_cursor = wxCursor(wxCURSOR_BLANK);
	
	this->m_timer_mainloop = new wxTimer(this);
	this->Bind(wxEVT_TIMER, &EngineCanvas::CameraControlMainloop, this);
	this->m_timer_mainloop->Start(10);

	this->Bind(wxEVT_PAINT, &EngineCanvas::Paint, this);
	this->Bind(wxEVT_IDLE, &EngineCanvas::RenderMainloop, this);
	this->Bind(wxEVT_KEY_DOWN, &EngineCanvas::KeyDown, this);
	this->Bind(wxEVT_LEFT_DOWN, &EngineCanvas::Clicked, this);
}

EngineCanvas::~EngineCanvas()
{
	this->m_timer_mainloop->Stop();
	delete this->m_timer_mainloop;
}

void EngineCanvas::Paint(wxPaintEvent& evt)
{
	this->GetEngine()->Render();
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
		int screen_centre[2] = { this->GetSize().x / 2, this->GetSize().y / 2 };
		int mouse_position[2];
		mouse_position[0] = mouse_state.GetPosition().x - this->GetScreenPosition().x;
		mouse_position[1] = mouse_state.GetPosition().y - this->GetScreenPosition().y;

		//check mouse delta and apply
		if (this->m_mouselook_active)
		{
			int mousedelta[2];
			mousedelta[0] = mouse_position[0] - screen_centre[0];
			mousedelta[1] = mouse_position[1] - screen_centre[1];

			float fov_fraction_x = ((float)mousedelta[0] * this->m_mouselook_multiplier) / (float)this->GetSize().x;
			float fov_fraction_y = ((float)mousedelta[1] * this->m_mouselook_multiplier) / (float)this->GetSize().x;

			float fov = (float)this->GetControlledCamera()->GetFOV();

			float rotation_z = fov_fraction_x * fov;
			float rotation_x = fov_fraction_y * fov;

			this->GetControlledCamera()->SetRotation(0, this->GetControlledCamera()->GetRotation(0) - rotation_x);
			this->GetControlledCamera()->SetRotation(2, this->GetControlledCamera()->GetRotation(2) - rotation_z);

			this->WarpPointer(screen_centre[0], screen_centre[1]);
		}

		//poll keyboard movement keys
		if (this->m_keyboard_move_active)
		{
			double move_increment = this->m_keyboard_move_increment;
			if (wxGetKeyState(WXK_SPACE))
			{
				move_increment = move_increment * 5.0;
			}

			if (wxGetKeyState(wxKeyCode('W')))
			{
				this->GetControlledCamera()->MoveLocally(0.0, 0.0, move_increment);
			}
			if (wxGetKeyState(wxKeyCode('S')))
			{
				this->GetControlledCamera()->MoveLocally(0.0, 0.0, 0.0 - move_increment);
			}
			if (wxGetKeyState(wxKeyCode('D')))
			{
				this->GetControlledCamera()->MoveLocally(0.0 - move_increment, 0.0, 0.0);
			}
			if (wxGetKeyState(wxKeyCode('A')))
			{
				this->GetControlledCamera()->MoveLocally(move_increment, 0.0, 0.0);
			}
			if (wxGetKeyState(WXK_CONTROL))
			{
				this->GetControlledCamera()->MoveLocally(0.0, move_increment, 0.0);
			}
			if (wxGetKeyState(WXK_SHIFT))
			{
				this->GetControlledCamera()->MoveLocally(0.0, 0.0 - move_increment, 0.0);
			}
		}
	}

	//restart timer
	this->m_timer_mainloop->Start();
	evt.Skip();
}

void EngineCanvas::RenderMainloop(wxIdleEvent& evt)
{
	if (this->m_loop_render)
	{
		this->GetEngine()->Render();
		evt.RequestMore();
	}
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
	this->MakeOpenGLFocus();
	this->m_camera_controlled->SetViewportDimensions(std::tuple(this->GetSize().x, this->GetSize().y));
}

void EngineCanvas::SetMouselookActive(bool enable)
{
	if (enable)
	{
		this->SetCursor(this->m_blank_cursor);
		this->WarpPointer(this->GetSize().x / 2, this->GetSize().y / 2);
	}
	else
	{
		this->SetCursor(wxNullCursor);
	}

	this->m_mouselook_active = enable;
}

void EngineCanvas::SetMouselook(bool enable)
{
	this->m_mouselook = enable;
	this->SetMouselookActive(enable);
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

void EngineCanvas::SetRenderLoop(bool enable)
{
	this->m_loop_render = enable;

	glFlush();
	this->SwapBuffers();
}

void EngineCanvas::SetVerticalSync(bool enabled)
{
#ifdef _WIN32 //both 32 and 64 bit windows
	wglSwapIntervalEXT(enabled);
#else
	throw std::runtime_error("Disabling/reenabling VSync isn't implemented outside of Windows");
#endif
}

std::tuple<int, int> EngineCanvas::GetOutputSize() const
{
	return std::tuple<int, int>(this->GetSize().x, this->GetSize().y);
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

void GLAPIENTRY MessageCallback(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar* message, const void* userParam)
{
	std::ofstream output_file;
	output_file.open(ENGINECANVAS_LOG_PATH);
	output_file << "(type: " << type << ", severity: ";

	if (severity == GL_DEBUG_SEVERITY_HIGH)
	{
		output_file << "high";
	}
	else if (severity == GL_DEBUG_SEVERITY_MEDIUM)
	{
		output_file << "medium";
	}
	else if (severity == GL_DEBUG_SEVERITY_LOW)
	{
		output_file << "low";
	}
	else if (severity == GL_DEBUG_SEVERITY_NOTIFICATION)
	{
		output_file << "notification";
	}
	else
	{
		output_file << severity;
	}
	output_file << "): " << message << std::endl;

	output_file.close();
	
	if ((type == GL_INVALID_ENUM
		|| type == GL_INVALID_VALUE
		|| type == GL_INVALID_OPERATION
		|| type == GL_STACK_OVERFLOW
		|| type == GL_STACK_UNDERFLOW
		|| type == GL_OUT_OF_MEMORY
		|| type == GL_INVALID_FRAMEBUFFER_OPERATION
		|| type == GL_TABLE_TOO_LARGE)
		|| (severity == GL_DEBUG_SEVERITY_HIGH))
	{
		throw std::runtime_error(message);
	}
}