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

	this->Bind(wxEVT_PAINT, &EngineCanvas::Paint, this);
	this->Bind(wxEVT_MOTION, &EngineCanvas::MouseMove, this);
	this->Render();
}

EngineCanvas::~EngineCanvas()
{
	wxDELETE(this->m_glcontext);
}

void EngineCanvas::Paint(wxPaintEvent& evt)
{
	this->Render();
	evt.Skip();
}

void EngineCanvas::MouseMove(wxMouseEvent& evt)
{
	if ((evt.Leaving()) || !(this->m_mouselook))
	{
		
	}
	else
	{
		int centre_coords[2] = { this->GetSize().x / 2, this->GetSize().y / 2 };
		this->WarpPointer(centre_coords[0], centre_coords[1]); //more responsive if we move it before rendering

		if (!(evt.Entering()))
		{
			int delta_coords[2] = { evt.GetPosition().x - centre_coords[0], evt.GetPosition().y - centre_coords[1] };

			if ((delta_coords[0] != 0) || (delta_coords[1] != 0))
			{
				float fov_fraction_x = (float)delta_coords[0] / (float)this->GetSize().x;
				float fov_fraction_y = (float)delta_coords[1] / (float)this->GetSize().x;

				float fov = this->m_scene->GetActiveCamera()->GetFOV();

				float rotation_z = fov_fraction_x * fov;
				float rotation_x = fov_fraction_y * fov;

				this->m_scene->GetActiveCamera()->SetRotation(0, this->m_scene->GetActiveCamera()->GetRotation(0) - rotation_x);
				this->m_scene->GetActiveCamera()->SetRotation(2, this->m_scene->GetActiveCamera()->GetRotation(2) - rotation_z);

				this->Render();
			}
		}

	}
	
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

void EngineCanvas::SetMouselook(bool enable)
{
	if (enable)
	{
		this->SetCursor(this->m_blank_cursor);
	}
	else
	{
		this->SetCursor(wxNullCursor);
	}

	this->m_mouselook = enable;
}

void EngineCanvas::SetScene(Scene* scene)
{
	this->m_scene = scene;
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