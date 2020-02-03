#include <wx/wxprec.h>
//#include <GL/glew.h>
#include "EngineCanvas.h"

EngineCanvas::EngineCanvas(wxWindow* parent, wxWindowID id, const int* args) : wxGLCanvas(parent, id, args)
{
	this->m_glcontext = new wxGLContext(this);
	this->SetCurrent(*this->m_glcontext);

	///////////////

	std::remove(ENGINECANVAS_LOG_PATH);

	glewExperimental = GL_TRUE;
	glLoadIdentity();
	glewInit();

	glEnable(GL_DEBUG_OUTPUT);
	glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
	glDebugMessageCallback(MessageCallback, 0);

	/*
	int success;
	char infoLog[512];
	int msglen;

	//vert shader
	const char* vertex_shader_src = "#version 400 core\n"
		"layout (location = 0) in vec3 aPos;\n"
		"out vec3 vpos;\n"
		"uniform mat4 model;\n"
		"uniform mat4 view;\n"
		"uniform mat4 projection;\n"
		"void main()\n"
		"{\n"
		"	gl_Position = projection * view * model * vec4(aPos.xyz, 1.0f);\n"
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
		"	gl_FragColor = vec4(vpos.xyz, 1.0);\n"
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

	
	//mats
	this->m_mdlmat = glm::mat4(1.0f);
	this->m_mdlmat = glm::rotate(this->m_mdlmat, glm::radians(-55.0f), glm::vec3(1.0f, 0.0f, 0.0f));

	this->m_viewmat = glm::mat4(1.0f);
	this->m_viewmat = glm::translate(this->m_viewmat, glm::vec3(0.0f, 0.0f, -3.0f));

	this->m_mdlmat_id = glGetUniformLocation(this->m_shader_program, "model");
	this->m_viewmat_id = glGetUniformLocation(this->m_shader_program, "view");
	this->m_perspmat_id = glGetUniformLocation(this->m_shader_program, "projection");

	glUniformMatrix4fv(this->m_mdlmat_id, 1, GL_FALSE, glm::value_ptr(this->m_mdlmat));
	glUniformMatrix4fv(this->m_viewmat_id, 1, GL_FALSE, glm::value_ptr(this->m_viewmat));

	//verts
	float verts[] = {
		-0.5f, -0.5f, 0.0f,
		0.5f, -0.5f, 0.0f,
		0.5f, 0.5f, 0.0f,
		0.5f, 0.5f, 0.0f,
		-0.5f, 0.5f, 0.0f,
		-0.5f, -0.5f, 0.0f,
	};

	GLuint vertex_buffer;
	glGenBuffers(1, &vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(verts), verts, GL_STATIC_DRAW);

	glDeleteShader(vertex_shader);
	glDeleteShader(frag_shader);

	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), (void*)0);
	glEnableVertexAttribArray(0);

	///////////////*/

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
	/*glClearColor(0, 1, 0, 0);
	glClear(GL_COLOR_BUFFER_BIT);
	glViewport(0, 0, (GLint)this->GetSize().x, (GLint)this->GetSize().y);

	this->m_perspmat = glm::perspective(glm::radians(45.0f), (float)this->GetSize().x / (float)this->GetSize().y, 0.1f, 100.0f);
	glUniformMatrix4fv(this->m_perspmat_id, 1, GL_FALSE, glm::value_ptr(this->m_perspmat));

	glUseProgram(this->m_shader_program);
	glBindVertexArray(this->m_VAO);
	glDrawArrays(GL_TRIANGLES, 0, 6);*/

	glViewport(0, 0, (GLint)this->GetSize().x, (GLint)this->GetSize().y);
	
	if (this->m_scene != nullptr)
	{
		this->m_scene->Render(this);
	}

	glFlush();
	this->SwapBuffers();
}

void EngineCanvas::SetScene(Scene* scene)
{
	this->m_scene = scene;
}

void GLAPIENTRY MessageCallback(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar* message, void* userParam)
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
		throw std::runtime_error("");
	}
}