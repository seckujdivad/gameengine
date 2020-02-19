#include <wx/wxprec.h>
#include "ShaderProgram.h"

ShaderProgram::ShaderProgram()
{
	this->m_program_id = NULL;
}

ShaderProgram::ShaderProgram(std::vector<std::tuple<std::string, GLenum>> shaders)
{
	this->m_program_id = glCreateProgram();
	if (this->m_program_id == NULL)
	{
		throw std::runtime_error("Couldn't create program object");
	}

	std::vector<GLuint> shader_ids;
	for (size_t i = 0; i < shaders.size(); i++)
	{
		shader_ids.push_back(this->LoadShader(std::get<0>(shaders.at(i)), std::get<1>(shaders.at(i))));
	}

	//link shaders
	for (size_t i = 0; i < shader_ids.size(); i++)
	{
		glAttachShader(this->m_program_id, shader_ids.at(i));
	}
	glLinkProgram(this->m_program_id);
	glUseProgram(this->m_program_id);

	//clean up shaders as they have already been linked
	for (size_t i = 0; i < shader_ids.size(); i++)
	{
		glDeleteShader(shader_ids.at(i));
	}

	//check for errors
	GLint link_was_successful; //should be glboolean in my opinion but that's what the function takes
	glValidateProgram(this->m_program_id);
	glGetProgramiv(this->m_program_id, GL_LINK_STATUS, &link_was_successful);
	if (link_was_successful != GL_TRUE) //get error message from GPU
	{
		char err_info[512];
		int err_len;
		glGetProgramInfoLog(this->m_program_id, 512, &err_len, err_info);
		std::string errmsg = std::string(err_info);
		errmsg = errmsg.substr(0, err_len);
		throw std::runtime_error("Shader link exception: " + errmsg);
	}
}

ShaderProgram::~ShaderProgram()
{
	if (this->m_program_id != NULL)
	{
		glDeleteProgram(this->m_program_id);
	}
}

GLuint ShaderProgram::LoadShader(std::string path, GLenum type)
{
	std::string shader_file_contents;
	std::ifstream shader_file;
	std::string line;

	//load shader from file
	shader_file_contents = "";
	shader_file.open(path);
	if (shader_file.is_open())
	{
		while (std::getline(shader_file, line))
		{
			shader_file_contents = shader_file_contents + line + "\n";
		}
	}
	else
	{
		throw std::runtime_error("Couldn't open shader file at \"" + path + "\"");
	}
	const char* shader_src = shader_file_contents.c_str();

	//load shader into GPU and compile
	GLuint shader_id = glCreateShader(type);
	glShaderSource(shader_id, 1, &shader_src, NULL);
	glCompileShader(shader_id);

	//check for errors
	GLint compile_was_successful; //should be glboolean in my opinion but that's what the function takes
	glGetShaderiv(shader_id, GL_COMPILE_STATUS, &compile_was_successful);
	if (compile_was_successful != GL_TRUE) //get error message from GPU
	{
		char err_info[512];
		int err_len;
		glGetShaderInfoLog(shader_id, 512, &err_len, err_info);
		std::string errmsg = std::string(err_info);
		errmsg = errmsg.substr(0, err_len);
		throw std::runtime_error("Shader compile exception (" + path + "): " + errmsg);
	}

	//return id
	return shader_id;
}

GLuint ShaderProgram::RegisterUniform(std::string name)
{
	if (this->m_program_id == NULL)
	{
		throw std::runtime_error("ShaderProgram has not been initialised");
	}
	else
	{
		GLuint uniform_id = glGetUniformLocation(this->m_program_id, name.c_str());
		this->m_uniforms.insert(std::pair<std::string, GLuint>(name, uniform_id));
		return uniform_id;
	}
}

void ShaderProgram::Select()
{
	if (this->m_program_id == NULL)
	{
		throw std::runtime_error("ShaderProgram has not been initialised");
	}
	else
	{
		glUseProgram(this->m_program_id);
	}
}

GLuint ShaderProgram::GetUniform(std::string name)
{
	if (this->m_program_id == NULL)
	{
		throw std::runtime_error("ShaderProgram has not been initialised");
	}
	else
	{
		return this->m_uniforms.at(name);
	}
}

GLuint ShaderProgram::GetProgramID()
{
	if (this->m_program_id == NULL)
	{
		throw std::runtime_error("ShaderProgram has not been initialised");
	}
	else
	{
		return this->m_program_id;
	}
}