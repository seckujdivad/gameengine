#pragma once

#include <string>
#include <fstream>
#include <vector>
#include <tuple>
#include <map>

#include "../GLComponents.h"
#include <glm/glm.hpp>

class ShaderProgram
{
private:
	GLuint LoadShader(std::string path, GLenum type);

	std::map<std::string, GLuint> m_uniforms;

public:
	GLuint program_id; //OpenGL identifier of the program the shaders have been linked into
	
	ShaderProgram(std::vector<std::tuple<std::string, GLenum>> shaders);
	~ShaderProgram();

	void Select();

	GLuint RegisterUniform(std::string name);
	GLuint GetUniform(std::string name);
};