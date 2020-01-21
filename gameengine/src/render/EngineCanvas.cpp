#include <wx/wxprec.h>
#include <GL/glew.h>
#include "EngineCanvas.h"

EngineCanvas::EngineCanvas(wxWindow* parent, wxWindowID id, const int* args) : wxGLCanvas(parent, id, args)
{
	this->m_glcontext = new wxGLContext(this);
	this->SetCurrent(*this->m_glcontext);

	///////////////

	glewExperimental = GL_TRUE;
	glLoadIdentity();
	glewInit();

	int success;
	char infoLog[512];
	int msglen;

	//vert shader
	const char* vertex_shader_src = "#version 400 core\n"
		"layout (location = 0) in vec3 aPos;\n"
		"out vec3 vpos;\n"
		"uniform float z_transform;"
		"void main()\n"
		"{\n"
		"	gl_Position = vec4(aPos.x + z_transform, aPos.y, aPos.z, 1.0f);\n"
		"	vpos = aPos;\n"
		"}\n";

	unsigned int vertex_shader = glCreateShader(GL_VERTEX_SHADER);
	glShaderSource(vertex_shader, 1, &vertex_shader_src, NULL);
	glCompileShader(vertex_shader);

	//frag shader
	const char* frag_shader_src = "#version 400 core\n"
		"out vec4 gl_FragColor;\n"
		"in vec3 vpos;\n"
		"void main()\n"
		"{\n"
		"	gl_FragColor = vec4(vpos.xyz, 1.0);\n"//vec4(1.0f, 0.5f, 0.2f, 1.0f);\n"
		"}\n";

	unsigned int frag_shader = glCreateShader(GL_FRAGMENT_SHADER);
	glShaderSource(frag_shader, 1, &frag_shader_src, NULL);
	glCompileShader(frag_shader);

	glGetShaderiv(frag_shader, GL_COMPILE_STATUS, &success);
	if (!success)
	{
		glGetShaderInfoLog(frag_shader, 512, NULL, infoLog);

	}

	//shader program
	this->m_shader_program = glCreateProgram();
	glAttachShader(this->m_shader_program, vertex_shader);
	glAttachShader(this->m_shader_program, frag_shader);
	glLinkProgram(this->m_shader_program);
	glUseProgram(this->m_shader_program);

	GLuint z_transform = glGetUniformLocation(this->m_shader_program, "z_transform");
	glUniform1f(z_transform, 0.0);

	// check for linking errors
	
	std::string k;
	glGetProgramiv(this->m_shader_program, GL_LINK_STATUS, &success);
	if (!success) {
		glGetProgramInfoLog(this->m_shader_program, sizeof(infoLog), &msglen, infoLog);
		k = std::string(infoLog);
	}
	k;

	unsigned int* vao = &this->m_VAO;
	glGenVertexArrays(1, vao);
	glBindVertexArray(this->m_VAO);

	//verts
	float verts[] = {
		-0.5f, -0.5f, 0.0f,
		0.5f, -0.5f, 0.0f,
		0.0f, 0.5f, 0.0f
	};

	GLuint vertex_buffer;
	GLuint* v = &vertex_buffer;
	glGenBuffers(1, v);
	glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(verts), verts, GL_STATIC_DRAW);

	glDeleteShader(vertex_shader);
	glDeleteShader(frag_shader);

	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), (void*)0);
	glEnableVertexAttribArray(0);

	///////////////

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
	GLfloat scale = 0.2;

	glClearColor(0, 1, 0, 0);
	glClear(GL_COLOR_BUFFER_BIT);
	glViewport(0, 0, (GLint)this->GetSize().x, (GLint)this->GetSize().y);

	
	/*glBegin(GL_POLYGON);
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
	glEnd();*/

	glUseProgram(this->m_shader_program);
	glBindVertexArray(this->m_VAO);
	glDrawArrays(GL_TRIANGLES, 0, 3);

	glFlush();
	this->SwapBuffers();
}

void EngineCanvas::SetScene(Scene* scene)
{
	
}