#pragma once

#include <string>

#include <GL/glew.h>

class ShaderProgram
{
private:
public:
	GLuint program_id;
	
	ShaderProgram(std::string vert_path, std::string frag_path, bool throw_errors = true);
	~ShaderProgram();

	void Select();

	void SetUniform(std::string name, bool value);
	void SetUniform(std::string name, int value);
	void SetUniform(std::string name, float value);
};