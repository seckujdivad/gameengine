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
	GLuint LoadShader(std::string path, GLenum type, std::vector<std::tuple<std::string, std::string>> preprocessor_defines);

	std::map<std::string, GLuint> m_uniforms;
	GLuint m_program_id = NULL; //OpenGL identifier of the program the shaders have been linked into

	int m_textures[16] = {
		-1, -1, -1, -1,
		-1, -1, -1, -1,
		-1, -1, -1, -1,
		-1, -1, -1, -1,
	};
	int m_texture_types[16] = {
		-1, -1, -1, -1,
		-1, -1, -1, -1,
		-1, -1, -1, -1,
		-1, -1, -1, -1,
	};
	std::string m_texture_names[16] = { "" };

	std::map<std::string, std::vector<void*>> m_shader_arrays;

public:
	ShaderProgram();
	ShaderProgram(std::vector<std::tuple<std::string, GLenum>> shaders, std::vector<std::tuple<std::string, std::string>> preprocessor_defines);
	ShaderProgram(const ShaderProgram& ) = delete; //This is a deliberate overload to throw an error. Copying of this object is not supported
	~ShaderProgram();

	void Select();

	GLuint GetProgramID();

	GLuint RegisterUniform(std::string name);
	GLuint GetUniform(std::string name);

	void LoadTexture(std::string name, unsigned char* data, int width, int height, int index = -1, GLuint min_filter = GL_NEAREST, GLuint mag_filter = GL_NEAREST);
	void RegisterTexture(std::string name, GLuint texture, GLuint type, int index = -1);
	void UpdateTexture(std::string name, GLuint texture);
	void UpdateTexture(int index, GLuint texture);

	int ReserveShaderArrayIndex(std::string array_name, void* object);
	int GetShaderArrayIndex(std::string array_name, void* object);
};