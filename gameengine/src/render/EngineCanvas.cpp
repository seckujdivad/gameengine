#include <wx/wxprec.h>
#include "EngineCanvas.h"

EngineCanvas::EngineCanvas(wxWindow* parent, wxWindowID id, wxGLAttributes& args) : wxGLCanvas(parent, args, id)
{
	wxGLContextAttrs ctx_attrs;
	ctx_attrs.PlatformDefaults().CoreProfile().MajorVersion(4).MinorVersion(0).EndList();
	this->m_glcontext = new wxGLContext(this, NULL, &ctx_attrs);
	this->SetCurrent(*this->m_glcontext);

	std::remove(ENGINECANVAS_LOG_PATH);
	
	glewExperimental = GL_TRUE;
	glewInit();
	glLoadIdentity();

	glEnable(GL_CULL_FACE);
	glEnable(GL_DEPTH_TEST);

	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_BLEND);

	glEnable(GL_DEBUG_OUTPUT);
	glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);

	glDebugMessageCallback(MessageCallback, 0);

	this->m_blank_cursor = wxCursor(wxCURSOR_BLANK);
	
	this->m_timer_mainloop = new wxTimer(this);
	this->Bind(wxEVT_TIMER, &EngineCanvas::CameraControlMainloop, this);
	this->m_timer_mainloop->Start(10);

	this->Bind(wxEVT_PAINT, &EngineCanvas::Paint, this);
	this->Bind(wxEVT_IDLE, &EngineCanvas::RenderMainloop, this);
	this->Bind(wxEVT_KEY_DOWN, &EngineCanvas::KeyDown, this);
	this->Bind(wxEVT_LEFT_DOWN, &EngineCanvas::Clicked, this);
	this->Render();
}

EngineCanvas::~EngineCanvas()
{
	this->m_timer_mainloop->Stop();
	delete this->m_timer_mainloop;
	wxDELETE(this->m_glcontext);
}

void EngineCanvas::Paint(wxPaintEvent& evt)
{
	this->Render();
	evt.Skip();
}

void EngineCanvas::Render()
{
	if (this->m_scene != nullptr)
	{
		glViewport(0, 0, this->GetSize().x, this->GetSize().y);
		this->m_scene->Render();
	}

	glFlush();
	this->SwapBuffers();
}

void EngineCanvas::SetScene(Scene* scene)
{
	this->m_scene = scene;
}

void EngineCanvas::CameraControlMainloop(wxTimerEvent& evt)
{
	//stop timer
	this->m_timer_mainloop->Stop();

	wxMouseState mouse_state = wxGetMouseState();
	int screen_centre[2] = { this->GetSize().x / 2, this->GetSize().y / 2 };
	int mouse_position[2];
	mouse_position[0] = mouse_state.GetPosition().x - this->GetScreenPosition().x;
	mouse_position[1] = mouse_state.GetPosition().y - this->GetScreenPosition().y;

	//poll keyboard movement keys
	if (this->m_keyboard_move_active)
	{
		if (wxGetKeyState(wxKeyCode('W')))
		{
			this->m_move_camera->MoveLocally(0.0f, 0.0f, this->m_keyboard_move_increment);
		}
		if (wxGetKeyState(wxKeyCode('S')))
		{
			this->m_move_camera->MoveLocally(0.0f, 0.0f, 0.0f - this->m_keyboard_move_increment);
		}
		if (wxGetKeyState(wxKeyCode('D')))
		{
			this->m_move_camera->MoveLocally(0.0f - this->m_keyboard_move_increment, 0.0f, 0.0f);
		}
		if (wxGetKeyState(wxKeyCode('A')))
		{
			this->m_move_camera->MoveLocally(this->m_keyboard_move_increment, 0.0f, 0.0f);
		}
		if (wxGetKeyState(WXK_CONTROL))
		{
			this->m_move_camera->MoveLocally(0.0f, this->m_keyboard_move_increment, 0.0f);
		}
		if (wxGetKeyState(WXK_SHIFT))
		{
			this->m_move_camera->MoveLocally(0.0f, 0.0f - this->m_keyboard_move_increment, 0.0f);
		}
	}

	//check mouse delta and apply
	if (this->m_mouselook_active)
	{
		int mousedelta[2];
		mousedelta[0] = mouse_position[0] - screen_centre[0];
		mousedelta[1] = mouse_position[1] - screen_centre[1];

		float fov_fraction_x = ((float)mousedelta[0] * this->m_mouselook_multiplier) / (float)this->GetSize().x;
		float fov_fraction_y = ((float)mousedelta[1] * this->m_mouselook_multiplier) / (float)this->GetSize().x;

		float fov = this->m_look_camera->GetFOV();

		float rotation_z = fov_fraction_x * fov;
		float rotation_x = fov_fraction_y * fov;

		this->m_look_camera->SetRotation(0, this->m_look_camera->GetRotation(0) - rotation_x);
		this->m_look_camera->SetRotation(2, this->m_look_camera->GetRotation(2) - rotation_z);

		this->WarpPointer(screen_centre[0], screen_centre[1]);
	}

	//restart timer
	this->m_timer_mainloop->Start();
	evt.Skip();
}

void EngineCanvas::RenderMainloop(wxIdleEvent& evt)
{
	if (this->m_loop_render)
	{
		this->Render();
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
	if (!this->m_mouselook_active)
	{
		this->SetMouselookActive(true);
		this->SetKeyboardMoveActive(true);
	}

	evt.Skip();
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

void EngineCanvas::SetMouselook(bool enable, Camera* camera)
{
	if (enable)
	{
		if ((camera == nullptr) && (this->m_look_camera == nullptr))
		{
			throw std::runtime_error("No camera specified and no camera stored");
		}
		else if (camera != nullptr)
		{
			this->m_look_camera = camera;
		}
	}
	
	this->m_mouselook = enable;
	this->SetMouselookActive(enable);
}

void EngineCanvas::SetKeyboardMove(bool enable, Camera* camera)
{
	if (enable)
	{
		if ((camera == nullptr) && (this->m_move_camera == nullptr))
		{
			throw std::runtime_error("No camera specified and no camera stored");
		}
		else if (camera != nullptr)
		{
			this->m_move_camera = camera;
		}
	}

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