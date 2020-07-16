#pragma once

#include <string>
#include <fstream>
#include <vector>
#include <tuple>
#include <map>

#include "../GLComponents.h"
#include <glm/glm.hpp>

const int GAMEENGINE_BOUND_TEXTURE_LIMIT = 16;

struct LoadedTexture
{
	GLuint id;
	GLenum type;
	std::string uniform_name;
};

class ShaderProgram
{
private:
	GLuint LoadShader(std::string path, GLenum type, std::vector<std::tuple<std::string, std::string>> preprocessor_defines, bool string_is_path);

	std::map<std::string, GLuint> m_uniforms;
	GLuint m_program_id = NULL; //OpenGL identifier of the program the shaders have been linked into

	std::map<int, std::vector<LoadedTexture>> m_textures;

	std::map<std::string, std::vector<void*>> m_shader_arrays;

public:
	ShaderProgram();
	ShaderProgram(std::vector<std::tuple<std::string, GLenum>> shaders, std::vector<std::tuple<std::string, std::string>> preprocessor_defines, bool strings_are_paths = true);
	ShaderProgram(const ShaderProgram& ) = delete; //This is a deliberate overload to throw an error. Copying of this object is not supported
	~ShaderProgram();

	void Select(int texture_group_id = NULL);

	GLuint GetProgramID();

	GLuint RegisterUniform(std::string name);
	GLuint GetUniform(std::string name);

	LoadedTexture LoadTexture(int texture_group_id, std::string registered_uniform, unsigned char* data, int width, int height, int index = -1, GLuint min_filter = GL_NEAREST, GLuint mag_filter = GL_NEAREST);
	void SetTexture(int texture_group_id, LoadedTexture texture);

	int ReserveShaderArrayIndex(std::string array_name, void* object);
	int GetShaderArrayIndex(std::string array_name, void* object);
};