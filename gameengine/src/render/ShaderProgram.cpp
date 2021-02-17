#include "ShaderProgram.h"

#include <stdexcept>

#include "../LogMessage.h"
#include "texture/Texture.h"
#include "TargetType.h"

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
	move_from.m_program_id = GL_NONE;

	this->m_max_texture_units = move_from.m_max_texture_units;

	this->m_sources = move_from.m_sources;
	this->m_uniforms = move_from.m_uniforms;
	this->m_textures = move_from.m_textures;
	this->m_defines = move_from.m_defines;

	return *this;
}

ShaderProgram::~ShaderProgram()
{
	if (this->m_program_id != GL_NONE)
	{
		glDeleteProgram(this->m_program_id);
	}
}

void ShaderProgram::Recompile(bool force)
{
	if (force || this->m_recompile_required)
	{
		if (this->m_program_id != GL_NONE)
		{
			glDeleteProgram(this->m_program_id);
		}

		this->m_program_id = glCreateProgram();
		if (this->m_program_id == GL_NONE)
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

		this->m_textures.clear();

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
	if (source.type == GL_NONE)
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
		processed_source = "#define " + key + " " + ShaderProgram::ConvertDefine<std::string>(value) + "\n" + processed_source;
	}

	processed_source = first_line + "\n" + processed_source;

	const char* shader_src = processed_source.c_str();

	//load shader into GPU and compile
	GLuint shader_id = glCreateShader(source.type);
	glShaderSource(shader_id, 1, &shader_src, 0);
	glCompileShader(shader_id);

	//get log
	{
		GLint buffer_len = 0;
		glGetShaderiv(shader_id, GL_INFO_LOG_LENGTH, &buffer_len);
		if (buffer_len > 1)
		{
			std::unique_ptr<GLchar[]> log_string = std::make_unique<GLchar[]>(std::size_t(buffer_len) + 1);
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
	if (this->m_program_id == GL_NONE)
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
		std::size_t num_textures = 0;
		for (int targeted_group : targeted_groups)
		{
			if (this->m_textures.count(targeted_group) > 0)
			{
				valid_targeted_groups.push_back(targeted_group);
				num_textures += this->m_textures.at(targeted_group).size();
			}
		}

		std::vector<GLTexture> textures;
		textures.reserve(num_textures);

		for (int targeted_group : valid_targeted_groups)
		{
			for (const auto& [uniform_name, texture] : this->m_textures.at(targeted_group))
			{
				GLTexture gl_texture;
				gl_texture.id = texture->GetTexture();
				gl_texture.target = GetTargetEnum(texture->GetTargetType());
				gl_texture.uniform_name = uniform_name;

				textures.push_back(gl_texture);
			}
		}

		if (static_cast<int>(textures.size()) >= this->m_max_texture_units)
		{
			throw std::runtime_error("Too many bound textures - maximum is " + std::to_string(this->m_max_texture_units));
		}

		for (int i = 0; i < static_cast<int>(textures.size()); i++)
		{
			glActiveTexture(GL_TEXTURE0 + i);
			BindOnlyThisTexture(textures.at(i));
			glUniform1i(this->GetUniform(textures.at(i).uniform_name), i);

#ifdef _DEBUG
			if ((textures.at(i).id != GL_NONE) && !glIsTexture(textures.at(i).id))
			{
				throw std::runtime_error("Texture does not exist (uniform \"" + textures.at(i).uniform_name + "\")");
			}
#endif
		}
	}

	glActiveTexture(GL_TEXTURE0);
}

GLuint ShaderProgram::GetUniform(std::string name)
{
	if (this->m_program_id == GL_NONE)
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
		if (this->m_program_id == GL_NONE)
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

void ShaderProgram::SetTexture(int texture_group_id, std::string uniform_name, Texture* texture)
{
	this->AddUniformName(uniform_name);

	bool add_new = false;
	if (this->m_textures.count(texture_group_id) == 0)
	{
		this->m_textures.insert(std::pair(texture_group_id, std::unordered_map<std::string, Texture*>()));
		add_new = true;
	}
	else
	{
		add_new = this->m_textures.at(texture_group_id).count(uniform_name) == 0;

		if (!add_new && (this->m_textures.at(texture_group_id).at(uniform_name) != texture))
		{
			this->m_textures.at(texture_group_id).at(uniform_name) = texture;
		}
	}

	if (add_new)
	{
		this->m_textures.at(texture_group_id).insert(std::pair(uniform_name, texture));
	}
}

bool ShaderProgram::SetDefine(std::string key, DefineType value, bool defer_recompilation)
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

template<>
ShaderProgram::DefineType ShaderProgram::GetDefine<ShaderProgram::DefineType>(std::string key) const
{
	return this->m_defines.at(key);
}

template<typename T>
T ShaderProgram::GetDefine(std::string key) const
{
	return this->ConvertDefine<T>(this->GetDefine<DefineType>(key));
}

template std::string ShaderProgram::GetDefine<std::string>(std::string key) const;
template int ShaderProgram::GetDefine<int>(std::string key) const;

template<>
bool ShaderProgram::DefineTypesMatch<std::string>(DefineType value)
{
	return value.index() == 0;
}

template<>
bool ShaderProgram::DefineTypesMatch<int>(DefineType value)
{
	return value.index() == 1;
}

template<typename T>
T ShaderProgram::ConvertDefine(DefineType value)
{
	if (ShaderProgram::DefineTypesMatch<T>(value))
	{
		return std::get<T>(value);
	}
	else
	{
		return ShaderProgram::ConvertDefine_Inner<T>(value);
	}
}

template<>
std::string ShaderProgram::ConvertDefine_Inner<std::string>(DefineType value)
{
	if (ShaderProgram::DefineTypesMatch<int>(value))
	{
		return std::to_string(std::get<int>(value));
	}
	else
	{
		throw std::invalid_argument("Invalid type");
	}
}

template<>
int ShaderProgram::ConvertDefine_Inner<int>(DefineType value)
{
	if (ShaderProgram::DefineTypesMatch<int>(value))
	{
		return std::stoi(std::get<std::string>(value));
	}
	else
	{
		throw std::invalid_argument("Invalid type");
	}
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
	if (this->m_program_id == GL_NONE)
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

void BindOnlyThisTexture(const ShaderProgram::GLTexture& texture)
{
	const GLenum targets[] = {
		GL_TEXTURE_2D,
		GL_TEXTURE_CUBE_MAP
	};

#ifdef _DEBUG
	bool target_found = false;
#endif

	for (GLenum target : targets)
	{
		glBindTexture(target, GL_NONE);

#ifdef _DEBUG
		if (target == texture.target)
		{
			target_found = true;
		}
#endif
	}

#ifdef _DEBUG
	if (!target_found)
	{
		throw std::invalid_argument("Unknown target: " + std::to_string(static_cast<int>(texture.target)));
	}
#endif

	glBindTexture(texture.target, texture.id);
}
