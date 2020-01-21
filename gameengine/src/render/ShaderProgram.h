#pragma once

#include <string>
#include <fstream>

#include <GL/glew.h>

class ShaderProgram
{
private:
public:
	GLuint program_id; //OpenGL identifier of the program the shaders have been linked into
	
	ShaderProgram(std::string vert_path, std::string frag_path);
	~ShaderProgram();

	void Select();

	void SetUniform(std::string name, bool value);
	void SetUniform(std::string name, int value);
	void SetUniform(std::string name, float value);
};