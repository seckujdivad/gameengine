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

	this->SetVerticalSync(false);

	//glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	//glEnable(GL_BLEND);

	glEnable(GL_DEBUG_OUTPUT);
	glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);

	glDebugMessageCallback(MessageCallback, 0);

	this->m_blank_cursor = wxCursor(wxCURSOR_BLANK);

	//create preprocessor vertices
	float vertices[] = {
		-1.0f, -1.0f, 0.0f,
		1.0f, -1.0f, 0.0f,
		1.0f, 1.0f, 0.0f,
		-1.0f, -1.0f, 0.0f,
		1.0f, 1.0f, 0.0f,
		- 1.0f, 1.0f, 0.0f
	};
	glGenVertexArrays(1, &this->m_postprocessor_vao);
	glBindVertexArray(this->m_postprocessor_vao);
	glGenBuffers(1, &this->m_postprocessor_vbo);
	glBindBuffer(GL_ARRAY_BUFFER, this->m_postprocessor_vbo);
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), (void*)0);
	glEnableVertexAttribArray(0);
	
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

	if (this->m_postprocessor != nullptr)
	{
		delete this->m_postprocessor;

		glDeleteTextures(1, &this->m_postprocessor_colour_texture_write);
		glDeleteTextures(1, &this->m_postprocessor_depth_texture_write);
		glDeleteTextures((GLsizei)this->m_postprocessor_data_textures_write.size(), this->m_postprocessor_data_textures_write.data());

		glDeleteTextures(1, &this->m_postprocessor_colour_texture_read);
		glDeleteTextures(1, &this->m_postprocessor_depth_texture_read);
		glDeleteTextures((GLsizei)this->m_postprocessor_data_textures_read.size(), this->m_postprocessor_data_textures_read.data());

		glDeleteFramebuffers(1, &this->m_postprocessor_fbo);
	}
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
		if (this->m_postprocessor == nullptr)
		{
			glViewport(0, 0, this->GetSize().x, this->GetSize().y);
			this->m_scene->Render(0);
		}
		else
		{
			if ((this->m_old_size[0] != this->GetSize().x) || (this->m_old_size[1] != this->GetSize().y))
			{
				glBindFramebuffer(GL_FRAMEBUFFER, this->m_postprocessor_fbo);

				glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_colour_texture_write);
				glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, this->GetSize().x, this->GetSize().y, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);

				glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_depth_texture_write);
				glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, this->GetSize().x, this->GetSize().y, 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
				glViewport(0, 0, this->GetSize().x, this->GetSize().y);

				for (int i = 0; i < (int)this->m_postprocessor_data_textures_write.size(); i++)
				{
					glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_data_textures_write.at(i));
					glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, this->GetSize().x, this->GetSize().y, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
				}

				glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_colour_texture_read);
				glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, this->GetSize().x, this->GetSize().y, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);

				glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_depth_texture_read);
				glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, this->GetSize().x, this->GetSize().y, 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
				glViewport(0, 0, this->GetSize().x, this->GetSize().y);

				for (int i = 0; i < (int)this->m_postprocessor_data_textures_read.size(); i++)
				{
					glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_data_textures_read.at(i));
					glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, this->GetSize().x, this->GetSize().y, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
				}

				this->m_old_size[0] = this->GetSize().x;
				this->m_old_size[1] = this->GetSize().y;
			}

			this->m_scene->Render(this->m_postprocessor_fbo);

			glBindFramebuffer(GL_FRAMEBUFFER, 0);
			glViewport(0, 0, this->GetSize().x, this->GetSize().y);
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

			this->m_postprocessor->Select();
			glBindVertexArray(this->m_postprocessor_vao);
			glDrawArrays(GL_TRIANGLES, 0, 6);

			//copy textures from the render textures to the readable textures
			glCopyImageSubData(this->m_postprocessor_colour_texture_write, GL_TEXTURE_2D, 0, 0, 0, 0,
				this->m_postprocessor_colour_texture_read, GL_TEXTURE_2D, 0, 0, 0, 0,
				this->GetSize().x, this->GetSize().y, 1);
			glCopyImageSubData(this->m_postprocessor_depth_texture_write, GL_TEXTURE_2D, 0, 0, 0, 0,
				this->m_postprocessor_depth_texture_read, GL_TEXTURE_2D, 0, 0, 0, 0,
				this->GetSize().x, this->GetSize().y, 1);
			for (int i = 0; i < (int)this->m_postprocessor_data_textures_read.size(); i++)
			{
				glCopyImageSubData(this->m_postprocessor_data_textures_write.at(i), GL_TEXTURE_2D, 0, 0, 0, 0,
					this->m_postprocessor_data_textures_read.at(i), GL_TEXTURE_2D, 0, 0, 0, 0,
					this->GetSize().x, this->GetSize().y, 1);
			}
		}
	}

	glFlush();
	this->SwapBuffers();
}

void EngineCanvas::SetScene(Scene* scene)
{
	this->m_scene = scene;
	this->m_scene->SetReceivedOutputTextures(this->m_postprocessor_colour_texture_read, this->m_postprocessor_depth_texture_read, this->m_postprocessor_data_textures_read);
}

void EngineCanvas::CameraControlMainloop(wxTimerEvent& evt)
{
	//stop timer
	this->m_timer_mainloop->Stop();

	//get mouse info
	wxMouseState mouse_state = wxGetMouseState();
	int screen_centre[2] = { this->GetSize().x / 2, this->GetSize().y / 2 };
	int mouse_position[2];
	mouse_position[0] = mouse_state.GetPosition().x - this->GetScreenPosition().x;
	mouse_position[1] = mouse_state.GetPosition().y - this->GetScreenPosition().y;

	//make sure the control still has focus
	if ((this->m_keyboard_move_active || this->m_mouselook_active) && !this->HasFocus())
	{
		this->SetMouselookActive(false);
		this->SetKeyboardMoveActive(false);
	}

	//poll keyboard movement keys
	if (this->m_keyboard_move_active)
	{
		float move_increment = this->m_keyboard_move_increment;
		if (wxGetKeyState(WXK_SPACE))
		{
			move_increment = move_increment * 5;
		}

		if (wxGetKeyState(wxKeyCode('W')))
		{
			this->m_move_camera->MoveLocally(0.0f, 0.0f, move_increment);
		}
		if (wxGetKeyState(wxKeyCode('S')))
		{
			this->m_move_camera->MoveLocally(0.0f, 0.0f, 0.0f - move_increment);
		}
		if (wxGetKeyState(wxKeyCode('D')))
		{
			this->m_move_camera->MoveLocally(0.0f - move_increment, 0.0f, 0.0f);
		}
		if (wxGetKeyState(wxKeyCode('A')))
		{
			this->m_move_camera->MoveLocally(move_increment, 0.0f, 0.0f);
		}
		if (wxGetKeyState(WXK_CONTROL))
		{
			this->m_move_camera->MoveLocally(0.0f, move_increment, 0.0f);
		}
		if (wxGetKeyState(WXK_SHIFT))
		{
			this->m_move_camera->MoveLocally(0.0f, 0.0f - move_increment, 0.0f);
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

void EngineCanvas::SetPostProcessorShaderProgram(ShaderProgram* postprocessor)
{
	this->m_postprocessor = postprocessor;
	
	//writeable textures (rendered to)
	glGenTextures(1, &this->m_postprocessor_colour_texture_write);
	glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_colour_texture_write);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, this->GetSize().x, this->GetSize().y, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	glGenTextures(1, &this->m_postprocessor_depth_texture_write);
	glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_depth_texture_write);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, this->GetSize().x, this->GetSize().y, 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	for (int i = 0; i < ENGINECANVAS_NUM_DATA_TEX; i++)
	{
		GLuint texture_id;
		glGenTextures(1, &texture_id);
		glBindTexture(GL_TEXTURE_2D, texture_id);
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, this->GetSize().x, this->GetSize().y, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

		this->m_postprocessor_data_textures_write.push_back(texture_id);
	}

	//readable textures (rendered from)
	glGenTextures(1, &this->m_postprocessor_colour_texture_read);
	glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_colour_texture_read);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, this->GetSize().x, this->GetSize().y, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	glGenTextures(1, &this->m_postprocessor_depth_texture_read);
	glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_depth_texture_read);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, this->GetSize().x, this->GetSize().y, 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	for (int i = 0; i < ENGINECANVAS_NUM_DATA_TEX; i++)
	{
		GLuint texture_id;
		glGenTextures(1, &texture_id);
		glBindTexture(GL_TEXTURE_2D, texture_id);
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, this->GetSize().x, this->GetSize().y, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

		this->m_postprocessor_data_textures_read.push_back(texture_id);
	}

	glGenFramebuffers(1, &this->m_postprocessor_fbo);
	glBindFramebuffer(GL_FRAMEBUFFER, this->m_postprocessor_fbo);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, this->m_postprocessor_colour_texture_write, 0);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, this->m_postprocessor_depth_texture_write, 0);

	for (int i = 0; i < (int)this->m_postprocessor_data_textures_write.size(); i++)
	{
		glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1 + i, GL_TEXTURE_2D, this->m_postprocessor_data_textures_write.at(i), 0);
	}

	std::vector<GLenum> buffers;
	for (int i = 0; i < ENGINECANVAS_NUM_DATA_TEX + 1; i++)
	{
		buffers.push_back(GL_COLOR_ATTACHMENT0 + i);
	}
	glDrawBuffers(ENGINECANVAS_NUM_DATA_TEX + 1, buffers.data());

	GLenum framebuffer_status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
	if (framebuffer_status != GL_FRAMEBUFFER_COMPLETE)
	{
		throw std::runtime_error("Framebuffer error, status " + std::to_string(framebuffer_status));
	}

	glBindFramebuffer(GL_FRAMEBUFFER, 0);

	this->m_postprocessor->RegisterTexture("render_output", this->m_postprocessor_colour_texture_read, GL_TEXTURE_2D);

	if (this->m_scene != nullptr)
	{
		this->m_scene->SetReceivedOutputTextures(this->m_postprocessor_colour_texture_read, this->m_postprocessor_depth_texture_read, this->m_postprocessor_data_textures_read);
	}
}

void EngineCanvas::SetVerticalSync(bool enabled)
{
#ifdef _WIN32 //both 32 and 64 bit windows
	wglSwapIntervalEXT(enabled);
#else
	throw std::runtime_error("Disabling/reenabling VSync isn't implemented outside of Windows");
#endif
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