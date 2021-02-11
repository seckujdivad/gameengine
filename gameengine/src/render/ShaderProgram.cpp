#include "ShaderProgram.h"

#include <stdexcept>

#include "../LogMessage.h"

ShaderProgram::ShaderProgram()
{
	glGetIntegerv(GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS, &this->m_max_texture_units); //get number of texture units allowed at once
}

ShaderProgram::ShaderProgram(ShaderProgram&& move_from) noexcept
{
	*this = std::move(move_from);
}

ShaderProgram& ShaderProgram::operator=(ShaderProgram&& move_from) noexcept
{
	if (this == &move_from)
	{
		return *this;
	}

	this->m_program_id = move_from.m_program_id;
	move_from.m_program_id = NULL;

	this->m_max_texture_units = move_from.m_max_texture_units;

	this->m_sources = move_from.m_sources;
	this->m_uniforms = move_from.m_uniforms;
	this->m_textures = move_from.m_textures;
	this->m_defines = move_from.m_defines;

	return *this;
}

ShaderProgram::~ShaderProgram()
{
	if (this->m_program_id != NULL)
	{
		glDeleteProgram(this->m_program_id);
	}
}

void ShaderProgram::Recompile(bool force)
{
	if (force || this->m_recompile_required)
	{
		if (this->m_program_id != NULL)
		{
			glDeleteProgram(this->m_program_id);
		}

		this->m_program_id = glCreateProgram();
		if (this->m_program_id == NULL)
		{
			throw std::runtime_error("Couldn't create program object");
		}

		std::vector<GLuint> shader_ids;
		for (const ShaderSource& shader_source : this->m_sources)
		{
			shader_ids.push_back(this->LoadShader(shader_source));
		}

		//link shaders
		for (GLuint shader_id : shader_ids)
		{
			glAttachShader(this->m_program_id, shader_id);
		}
		glLinkProgram(this->m_program_id);

		//clean up shaders as they have already been linked
		for (GLuint shader_id : shader_ids)
		{
			glDetachShader(this->m_program_id, shader_id);
			glDeleteShader(shader_id);
		}

		//check for errors
		GLint link_was_successful; //should be glboolean in my opinion but that's what the function takes
		glGetProgramiv(this->m_program_id, GL_LINK_STATUS, &link_was_successful);
		if (link_was_successful != GL_TRUE) //get error message from GPU
		{
			std::string errmsg = this->GetInfoLog();
			LogMessage("Shader link exception: " + errmsg);
			throw std::runtime_error("Shader link exception: " + errmsg);
		}

		//re-register uniform names
		std::vector<std::string> uniform_names;
		for (const auto& [name, id] : this->m_uniforms)
		{
			uniform_names.push_back(name);
		}

		this->m_uniforms.clear();
		this->AddUniformNames(uniform_names);

		this->Select();

		this->m_recompile_required = false;
	}
}

bool ShaderProgram::RecompileIsRequired() const
{
	return this->m_recompile_required;
}

void ShaderProgram::SetShaderSources(std::vector<ShaderSource> sources, bool defer_recompilation)
{
	this->m_sources = sources;

	this->m_recompile_required = true;
	if (!defer_recompilation)
	{
		this->Recompile();
	}
}

GLuint ShaderProgram::LoadShader(ShaderSource source)
{
	if (source.type == NULL)
	{
		throw std::invalid_argument("ShaderSource::type must be set to a valid OpenGL shader enumerator");
	}

	size_t newline_pivot = source.source.find('\n');
	std::string first_line = source.source.substr(0, newline_pivot);

	std::string processed_source = source.source.substr(newline_pivot);
	processed_source = "#pragma optionNV(strict on)\n" + processed_source;
	processed_source = "#define __GL_ShaderPortabilityWarnings 1\n" + processed_source;
	processed_source = "#define __GL_WriteInfoLog 1\n" + processed_source;

	//add preprocessor defines
	for (const auto& [key, value] : this->m_defines)
	{
		processed_source = "#define " + key + " " + value + "\n" + processed_source;
	}

	processed_source = first_line + "\n" + processed_source;

	const char* shader_src = processed_source.c_str();

	//load shader into GPU and compile
	GLuint shader_id = glCreateShader(source.type);
	glShaderSource(shader_id, 1, &shader_src, NULL);
	glCompileShader(shader_id);

	//get log
	{
		GLint buffer_len = NULL;
		glGetShaderiv(shader_id, GL_INFO_LOG_LENGTH, &buffer_len);
		if (buffer_len > 1)
		{
			std::unique_ptr<GLchar[]> log_string = std::make_unique<GLchar[]>(buffer_len + 1);
			glGetShaderInfoLog(shader_id, buffer_len, 0, log_string.get());

			LogMessage("Shader log: " + std::string(log_string.get()));
		}
	}

	//check for errors
	GLint compile_was_successful; //should be glboolean in my opinion but that's what the function takes
	glGetShaderiv(shader_id, GL_COMPILE_STATUS, &compile_was_successful);
	if (compile_was_successful != GL_TRUE) //get error message from GPU
	{
		const int error_length = 512;
		char err_info[error_length];
		int err_len;
		glGetShaderInfoLog(shader_id, error_length, &err_len, err_info);
		std::string errmsg = std::string(err_info);
		errmsg = errmsg.substr(0, err_len);

		std::string final_message = "Shader compile exception: " + errmsg;

		LogMessage(final_message);
		throw std::runtime_error(final_message);
	}

	//return id
	return shader_id;
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

		std::vector<int> valid_targeted_groups;
		std::size_t num_textures = 1;
		for (int targeted_group : targeted_groups)
		{
			if (this->m_textures.count(targeted_group) > 0)
			{
				valid_targeted_groups.push_back(targeted_group);
				num_textures += this->m_textures.at(targeted_group).size();
			}
		}

		std::vector<LoadedTexture> textures;
		textures.reserve(num_textures);

		/*
		* Without an invalid dummy texture in texture unit 0, OpenGL will produce an invalid program texture usage error
		* This is a short term fix for the problem. I suspect that it has something to do with the default behaviour of
		* texture unit 0 - it is automatically activated and uninitialised texture uniforms will use it. Based on this
		* logic, there is an unlinked texture somewhere (maybe the skybox texture?) that is linking to whatever is in
		* texture unit 0.
		* 
		* If my reasoning is correct, having a dummy texture could be useful to prevent unlinked textures from generating
		* errors. Either way, there are more pressing issues than this.
		* 
		* TODO: remove dummy texture requirement
		*/
		{
			LoadedTexture dummy;
			dummy.id = NULL;
			dummy.type = GL_TEXTURE_2D;
			textures.push_back(dummy);
		}

		for (int targeted_group : valid_targeted_groups)
		{
			for (LoadedTexture& texture : this->m_textures.at(targeted_group))
			{
				textures.push_back(texture);
			}
		}

		if (static_cast<int>(textures.size()) >= this->m_max_texture_units)
		{
			throw std::runtime_error("Too many bound textures - maximum is " + std::to_string(this->m_max_texture_units));
		}

		for (int i = 0; i < static_cast<int>(textures.size()); i++)
		{
			glActiveTexture(GL_TEXTURE0 + i);
			glBindTexture(textures.at(i).type, textures.at(i).id);
			glUniform1i(this->GetUniform(textures.at(i).uniform_name), i);

#ifdef _DEBUG
			if ((textures.at(i).id != 0) && !glIsTexture(textures.at(i).id))
			{
				throw std::runtime_error("Texture does not exist (uniform \"" + textures.at(i).uniform_name + "\")");
			}
#endif
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
		GLuint result;
		if (this->m_uniforms.count(name) == 0)
		{
			result = this->AddUniformName(name);
		}
		else
		{
			result = this->m_uniforms.at(name);
		}

#ifdef GM_SHADER_REJECT_UNKNOWN_UNIFORMS
		if (result == static_cast<GLuint>(-1))
		{
			throw std::invalid_argument("Uniform location stored is -1 - this is not allowed");
		}
#endif

		return result;
	}
}

GLuint ShaderProgram::AddUniformName(std::string name)
{
	if (this->m_uniforms.count(name) == 0)
	{
		if (this->m_program_id == NULL)
		{
			throw std::runtime_error("ShaderProgram has not been initialised");
		}
		else if (name == "")
		{
#ifdef GM_SHADER_REJECT_UNKNOWN_UNIFORMS
			throw std::invalid_argument("Shader uniform can't be blank");
#else
			return static_cast<GLuint>(-1);
#endif
		}
		else if (this->m_uniforms.count(name) == 0)
		{
			glUseProgram(this->m_program_id);
			GLuint uniform_id = glGetUniformLocation(this->m_program_id, name.c_str());

#ifdef GM_SHADER_REJECT_UNKNOWN_UNIFORMS
			if (uniform_id == static_cast<GLuint>(-1))
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
	else
	{
		return this->m_uniforms.at(name);
	}
}

std::vector<GLuint> ShaderProgram::AddUniformNames(std::vector<std::string> names)
{
	std::vector<GLuint> result;
	result.reserve(names.size());
	for (std::string name : names)
	{
		result.push_back(this->AddUniformName(name));
	}
	return result;
}

void ShaderProgram::RemoveUniform(std::string name)
{
	this->m_uniforms.erase(name);
}

void ShaderProgram::RemoveUniforms(std::vector<std::string> names)
{
	for (std::string name : names)
	{
		this->RemoveUniform(name);
	}
}

void ShaderProgram::SetUniform(std::string name, bool value)
{
	glUniform1i(this->GetUniform(name), value ? GL_TRUE : GL_FALSE);
}

void ShaderProgram::SetUniform(std::string name, int value)
{
	glUniform1i(this->GetUniform(name), value);
}

void ShaderProgram::SetUniform(std::string name, float value)
{
	glUniform1f(this->GetUniform(name), value);
}

void ShaderProgram::SetUniform(std::string name, double value, bool demote)
{
	if (demote)
	{
		this->SetUniform(name, static_cast<float>(value));
	}
	else
	{
		glUniform1d(this->GetUniform(name), value);
	}
}

void ShaderProgram::SetUniform(std::string name, glm::vec2 vec)
{
	glUniform2fv(this->GetUniform(name), 1, glm::value_ptr(vec));
}

void ShaderProgram::SetUniform(std::string name, glm::dvec2 vec, bool demote)
{
	if (demote)
	{
		this->SetUniform(name, glm::vec2(vec));
	}
	else
	{
		glUniform2dv(this->GetUniform(name), 1, glm::value_ptr(vec));
	}
}

void ShaderProgram::SetUniform(std::string name, glm::vec3 vec)
{
	glUniform3fv(this->GetUniform(name), 1, glm::value_ptr(vec));
}

void ShaderProgram::SetUniform(std::string name, glm::dvec3 vec, bool demote)
{
	if (demote)
	{
		this->SetUniform(name, glm::vec3(vec));
	}
	else
	{
		glUniform3dv(this->GetUniform(name), 1, glm::value_ptr(vec));
	}
}

void ShaderProgram::SetUniform(std::string name, glm::vec4 vec)
{
	glUniform4fv(this->GetUniform(name), 1, glm::value_ptr(vec));
}

void ShaderProgram::SetUniform(std::string name, glm::dvec4 vec, bool demote)
{
	if (demote)
	{
		this->SetUniform(name, glm::vec4(vec));
	}
	else
	{
		glUniform4dv(this->GetUniform(name), 1, glm::value_ptr(vec));
	}
}

void ShaderProgram::SetUniform(std::string name, glm::mat4 mat)
{
	glUniformMatrix4fv(this->GetUniform(name), 1, GL_FALSE, glm::value_ptr(mat));
}

void ShaderProgram::SetUniform(std::string name, glm::dmat4 mat, bool demote)
{
	if (demote)
	{
		this->SetUniform(name, glm::mat4(mat));
	}
	else
	{
		glUniformMatrix4dv(this->GetUniform(name), 1, GL_FALSE, glm::value_ptr(mat));
	}
}

void ShaderProgram::SetUniform(std::string name, glm::mat3 mat)
{
	glUniformMatrix3fv(this->GetUniform(name), 1, GL_FALSE, glm::value_ptr(mat));
}

void ShaderProgram::SetUniform(std::string name, glm::dmat3 mat, bool demote)
{
	if (demote)
	{
		this->SetUniform(name, glm::mat3(mat));
	}
	else
	{
		glUniformMatrix3dv(this->GetUniform(name), 1, GL_FALSE, glm::value_ptr(mat));
	}
}

void ShaderProgram::SetTexture(int texture_group_id, LoadedTexture texture)
{
	this->AddUniformName(texture.uniform_name);

	if (this->m_textures.find(texture_group_id) == this->m_textures.end())
	{
		this->m_textures.insert(std::pair(texture_group_id, std::vector<LoadedTexture>()));
		this->m_textures.at(texture_group_id).push_back(texture);
	}
	else
	{
		bool found_match = false;
		std::vector<LoadedTexture>& textures = this->m_textures.at(texture_group_id);
		for (std::size_t i = 0; (i < textures.size()) && !found_match; i++)
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
	}
}

bool ShaderProgram::SetDefine(std::string key, std::string value, bool defer_recompilation)
{
	bool recompile_required = false;

	auto it = this->m_defines.find(key);
	if (it == this->m_defines.end())
	{
		this->m_defines.insert(std::pair(key, value));

		recompile_required = true;
	}
	else if (it->second != value)
	{
		this->m_defines.at(key) = value;

		recompile_required = true;
	}
	
	if (recompile_required)
	{
		this->m_recompile_required = true;
		if (!defer_recompilation)
		{
			this->Recompile();
		}
	}

	return recompile_required && defer_recompilation;
}

bool ShaderProgram::RemoveDefine(std::string key, bool defer_recompilation)
{
	bool recompile_required = false;

	if (this->m_defines.count(key) != 0)
	{
		this->m_defines.erase(key);

		recompile_required = true;
	}

	if (recompile_required)
	{
		this->m_recompile_required = true;
		if (!defer_recompilation)
		{
			this->Recompile();
		}
	}

	return recompile_required && defer_recompilation;
}

std::optional<std::string> ShaderProgram::CheckProgramValidity() const
{
	if (this->IsValid())
	{
		return std::optional<std::string>();
	}
	else
	{
		return this->GetInfoLog();
	}
}

std::string ShaderProgram::GetInfoLog() const
{
	const int max_err_len = 512;
	int err_len = 0;
	char err_info[max_err_len];
	glGetProgramInfoLog(this->m_program_id, max_err_len, &err_len, err_info);
	std::string errmsg = std::string(err_info);
	errmsg = errmsg.substr(0, err_len);
	return errmsg;
}

bool ShaderProgram::IsValid() const
{
	glValidateProgram(this->m_program_id);
	GLint program_is_valid = GL_FALSE;
	glGetProgramiv(this->m_program_id, GL_VALIDATE_STATUS, &program_is_valid);
	return program_is_valid == GL_TRUE;
}

GLuint ShaderProgram::GetProgramID() const
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

ShaderProgram::ShaderSource::ShaderSource(std::string source, GLenum type) : source(source), type(type)
{
}

ShaderProgram::ShaderSource::ShaderSource(GLenum type, std::string source) : type(type), source(source)
{
}
