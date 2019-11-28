#include "EngineCanvas.h"

EngineCanvas::EngineCanvas(wxWindow* parent, wxWindowID id) : wxGLCanvas(parent, id)
{
	this->m_glcontext = new wxGLContext(this);
	
	this->Bind(wxEVT_PAINT, &EngineCanvas::Paint, this);
	this->Render();
}

EngineCanvas::~EngineCanvas()
{
	delete this->m_glcontext;
}

void EngineCanvas::Paint(wxPaintEvent& evt)
{
	this->Render();
	evt.Skip();
}

void EngineCanvas::Render()
{
	float scale = 0.2;

	this->SetCurrent(*this->m_glcontext);

	glClearColor(0, 0, 0, 0);
	glClear(GL_COLOR_BUFFER_BIT);
	glViewport(0, 0, (GLint)this->GetSize().x, (GLint)this->GetSize().y);

	glBegin(GL_POLYGON);
	glColor3f(1.0, 1.0, 1.0);
	glVertex2f(-0.5, -0.5);
	glVertex2f(-0.5, 0.5);
	glVertex2f(0.5, 0.5);
	glVertex2f(0.5, -0.5);
	glColor3f(0.4, 0.5, 0.4);
	glVertex2f(0.0, -0.8);
	glEnd();

	glBegin(GL_POLYGON);
	glColor3f(0.5, 0.5, 0.5);
	glVertex2f(-scale, -scale);
	glVertex2f(scale, -scale);
	glVertex2f(scale, scale);
	glVertex2f(-scale, scale);
	glEnd();

	glFlush();
	this->SwapBuffers();
}