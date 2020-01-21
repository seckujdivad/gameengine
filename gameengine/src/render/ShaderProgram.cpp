#include <wx/wxprec.h>
#include "ShaderProgram.h"

ShaderProgram::ShaderProgram(std::string vert_path, std::string frag_path)
{
	std::string shader_file_contents;
	std::ifstream shader_file;
	std::string line;
	
	//load vertex shader
	shader_file_contents = "";
	shader_file.open(vert_path);
	if (shader_file.is_open())
	{
		while (std::getline(shader_file, line))
		{
			shader_file_contents = shader_file_contents + line + "\n";
		}
	}
	else
	{
		throw std::runtime_error("Couldn't open vertex shader file at \"" + vert_path + "\"");
	}
}

ShaderProgram::~ShaderProgram()
{

}

void ShaderProgram::Select()
{

}

void ShaderProgram::SetUniform(std::string name, bool value)
{

}

void ShaderProgram::SetUniform(std::string name, int value)
{

}

void ShaderProgram::SetUniform(std::string name, float value)
{

}