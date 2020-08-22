#include <wx/wxprec.h>
#include "ShaderProgram.h"

ShaderProgram::ShaderProgram()
{
	this->m_program_id = NULL;
}

ShaderProgram::ShaderProgram(std::vector<std::tuple<std::string, GLenum>> shaders, std::vector<std::tuple<std::string, std::string>> preprocessor_defines, bool strings_are_paths)
{
	this->m_program_id = glCreateProgram();
	if (this->m_program_id == NULL)
	{
		throw std::runtime_error("Couldn't create program object");
	}

	std::vector<GLuint> shader_ids;
	for (size_t i = 0; i < shaders.size(); i++)
	{
		shader_ids.push_back(this->LoadShader(std::get<0>(shaders.at(i)), std::get<1>(shaders.at(i)), preprocessor_defines, strings_are_paths));
	}

	//link shaders
	for (size_t i = 0; i < shader_ids.size(); i++)
	{
		glAttachShader(this->m_program_id, shader_ids.at(i));
	}
	glLinkProgram(this->m_program_id);

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

GLuint ShaderProgram::LoadShader(std::string path, GLenum type, std::vector<std::tuple<std::string, std::string>> preprocessor_defines, bool string_is_path)
{
	std::string shader_file_contents;
	std::ifstream shader_file;
	std::string line;
	std::string line0;

	if (string_is_path)
	{
		//load shader from file
		shader_file_contents = "";
		shader_file.open(path);
		if (shader_file.is_open())
		{
			while (std::getline(shader_file, line))
			{
				if (line0 == "")
				{
					line0 = line;
				}
				else
				{
					shader_file_contents = shader_file_contents + line + "\n";
				}
			}
		}
		else
		{
			throw std::runtime_error("Couldn't open shader file at \"" + path + "\"");
		}
	}
	else
	{
		size_t newline_pivot = path.find('\n');
		line0 = path.substr(0, newline_pivot);
		shader_file_contents = path.substr(newline_pivot);

	}

	//add preprocessor defines
	for (int i = 0; i < (int)preprocessor_defines.size(); i++)
	{
		shader_file_contents = "#define " + std::get<0>(preprocessor_defines.at(i)) + " " + std::get<1>(preprocessor_defines.at(i)) + "\n" + shader_file_contents;
	}

	shader_file_contents = line0 + "\n" + shader_file_contents;

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
	else if (this->m_uniforms.find(name) == this->m_uniforms.end())
	{
		glUseProgram(this->m_program_id);
		GLuint uniform_id = glGetUniformLocation(this->m_program_id, name.c_str());

#ifdef GM_SHADER_REJECT_UNKNOWN_UNIFORMS
		if (uniform_id == (GLuint)(-1))
		{
			throw std::runtime_error("OpenGL returned -1 for uniform location - this is not allowed");
		}
#endif

		this->m_uniforms.insert(std::pair<std::string, GLuint>(name, uniform_id));
		return uniform_id;
	}
	else
	{
		return this->m_uniforms.at(name);
	}
}

void ShaderProgram::Select(int texture_group_id)
{
	if (this->m_program_id == NULL)
	{
		throw std::runtime_error("ShaderProgram has not been initialised");
	}
	else
	{
		glUseProgram(this->m_program_id);

		std::vector<int> targeted_groups;
		targeted_groups.push_back(-1);
		if (texture_group_id != -1)
		{
			targeted_groups.push_back(texture_group_id);
		}

		for (int group : targeted_groups)
		{
			auto it = this->m_textures.find(group);
			if (it != this->m_textures.end())
			{
				std::vector<LoadedTexture> textures = it->second;
				for (int i = 0; i < (int)textures.size(); i++)
				{
					glActiveTexture(GL_TEXTURE0 + ((GL_TEXTURE1 - GL_TEXTURE0) * i));
					glBindTexture(textures.at(i).type, textures.at(i).id);
					glUniform1i(this->GetUniform(textures.at(i).uniform_name), i);
				}
			}
		}
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
		auto it = this->m_uniforms.find(name);

		GLuint result;
		if (it == this->m_uniforms.end())
		{
			result = this->RegisterUniform(name);
		}
		else
		{
			result = it->second;
		}

#ifdef GM_SHADER_REJECT_UNKNOWN_UNIFORMS
		if (result == (GLuint)(-1))
		{
			throw std::runtime_error("Uniform location stored is -1 - this is not allowed");
		}
#endif

		return result;
	}
}

LoadedTexture ShaderProgram::LoadTexture(int texture_group_id, std::string registered_uniform, unsigned char* data, int width, int height, int index, GLuint min_filter, GLuint mag_filter)
{
	this->Select();

	GLuint texture;
	glGenTextures(1, &texture);
	glBindTexture(GL_TEXTURE_2D, texture);

	//wrapping
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

	//filter
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, min_filter); //shrinking filter
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, mag_filter); //enlarging filter

	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, data);
	glGenerateMipmap(GL_TEXTURE_2D);

	LoadedTexture texture_data;
	texture_data.id = texture;
	texture_data.type = GL_TEXTURE_2D;
	texture_data.uniform_name = registered_uniform;

	this->SetTexture(texture_group_id, texture_data);

	return texture_data;
}

void ShaderProgram::SetTexture(int texture_group_id, LoadedTexture texture)
{
	if (this->m_textures.find(texture_group_id) == this->m_textures.end())
	{
		this->m_textures.insert({ texture_group_id, std::vector<LoadedTexture>({texture}) });
	}
	else
	{
		bool found_match = false;
		std::vector<LoadedTexture> textures = this->m_textures.at(texture_group_id);
		for (int i = 0; (i < (int)textures.size()) && !found_match; i++)
		{
			if (textures.at(i).uniform_name == texture.uniform_name)
			{
				found_match = true;
				textures.at(i) = texture;
			}
		}

		if (!found_match)
		{
			textures.push_back(texture);
		}

		this->m_textures.at(texture_group_id) = textures;
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

int ShaderProgram::ReserveShaderArrayIndex(std::string array_name, void* object)
{
	if (this->m_shader_arrays.count(array_name) == 0)
	{
		this->m_shader_arrays.insert(std::pair<std::string, std::vector<void*>>(array_name, std::vector<void*>()));
		this->m_shader_arrays.at(array_name).push_back(object);
		return 0;
	}
	else
	{
		std::vector<void*> object_pointers = this->m_shader_arrays.at(array_name);
		std::vector<void*>::iterator it = std::find(object_pointers.begin(), object_pointers.end(), object);

		if (it == object_pointers.end())
		{
			this->m_shader_arrays.at(array_name).push_back(object);
			return (int)(this->m_shader_arrays.at(array_name).size() - 1);
		}
		else
		{
			return std::distance(object_pointers.begin(), it);
		}
	}
}

int ShaderProgram::GetShaderArrayIndex(std::string array_name, void* object)
{
	if (this->m_shader_arrays.count(array_name) == 0)
	{
		throw std::runtime_error("Array name not registered with ShaderProgram::ReserveShaderArrayIndex");
	}
	else
	{
		std::vector<void*> object_pointers = this->m_shader_arrays.at(array_name);
		std::vector<void*>::iterator it = std::find(object_pointers.begin(), object_pointers.end(), object);

		if (it == object_pointers.end())
		{
			throw std::runtime_error("Pointer not registered with this array with ShaderProgram::ReserveShaderArrayIndex");
		}
		else
		{
			return std::distance(object_pointers.begin(), it);
		}
	}
}
