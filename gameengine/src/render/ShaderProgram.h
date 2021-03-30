#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <optional>
#include <variant>
#include <type_traits>

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "../GLComponents.h"

class GLTexture;

const int GAMEENGINE_BOUND_TEXTURE_LIMIT = 16;

class ShaderProgram
{
public:
	struct ShaderSource
	{
		ShaderSource(std::string source, GLenum type);
		ShaderSource(GLenum type, std::string source);

		std::string source;
		GLenum type = GL_NONE;
	};

	using DefineType = std::variant<std::string, int>;

private:
	struct GLTextureUnmanaged
	{
		GLuint id = GL_NONE;
		GLenum target = GL_NONE;
		std::string uniform_name;

		void Bind() const;
	};

	GLuint m_program_id = GL_NONE; //OpenGL identifier of the program the shaders have been linked into
	GLint m_max_texture_units = 16; //minimum value required by the spec

	std::vector<ShaderSource> m_sources;

	std::unordered_map<std::string, GLuint> m_uniforms;
	std::unordered_map<int, std::unordered_map<std::string, GLTexture*>> m_textures;
	std::unordered_map<std::string, DefineType> m_defines;

	bool m_recompile_required = true;

	GLuint LoadShader(ShaderSource source);

	std::string GetInfoLog() const;
	bool IsValid() const;

	template<typename T>
	static T ConvertDefine_Inner(DefineType value);

public:
	ShaderProgram();
	ShaderProgram(const ShaderProgram&) = delete; //This is a deliberate overload to throw an error. Copying of this object is not supported
	ShaderProgram& operator=(const ShaderProgram&) = delete;
	ShaderProgram(ShaderProgram&& move_from) noexcept;
	ShaderProgram& operator=(ShaderProgram&& move_from) noexcept;
	~ShaderProgram();

	bool Recompile(bool force = false);
	bool RecompileIsRequired() const;

	void SetShaderSources(std::vector<ShaderSource> sources, bool defer_recompilation = true);
	GLuint GetProgramID() const;

	void Select(int texture_group_id = -1);
	GLuint GetUniform(std::string name);
	
	GLuint AddUniformName(std::string name);
	std::vector<GLuint> AddUniformNames(std::vector<std::string> names);
	void RemoveUniform(std::string name);
	void RemoveUniforms(std::vector<std::string> names);

	void SetUniform(std::string name, bool value);
	void SetUniform(std::string name, int value);
	void SetUniform(std::string name, float value);
	void SetUniform(std::string name, double value, bool demote = true);
	void SetUniform(std::string name, glm::ivec2 vec);
	void SetUniform(std::string name, glm::vec2 vec);
	void SetUniform(std::string name, glm::dvec2 vec, bool demote = true);
	void SetUniform(std::string name, glm::ivec3 vec);
	void SetUniform(std::string name, glm::vec3 vec);
	void SetUniform(std::string name, glm::dvec3 vec, bool demote = true);
	void SetUniform(std::string name, glm::ivec4 vec);
	void SetUniform(std::string name, glm::vec4 vec);
	void SetUniform(std::string name, glm::dvec4 vec, bool demote = true);
	void SetUniform(std::string name, glm::mat4 mat);
	void SetUniform(std::string name, glm::dmat4 mat, bool demote = true);
	void SetUniform(std::string name, glm::mat3 mat);
	void SetUniform(std::string name, glm::dmat3 mat, bool demote = true);

	template<class Iterator, typename = std::enable_if_t<std::is_base_of_v<std::forward_iterator_tag, typename std::iterator_traits<Iterator>::iterator_category>>>
	inline void SetUniform(std::string prefix, Iterator begin, Iterator end, int index_start = 0, int index_step = 1)
	{
		int i = 0;
		for (auto it = begin; it != end; ++it)
		{
			this->SetUniform(prefix + "[" + std::to_string(index_start + (i * index_step)) + "]", *it);
			i++;
		}
	}

	template<typename T>
	inline void SetUniform(std::string prefix, std::vector<T> values, int index_start = 0, int index_step = 1)
	{
		this->SetUniform(prefix, values.begin(), values.end(), index_start, index_step);
	};

	void SetTexture(int texture_group_id, std::string uniform_name, GLTexture* texture);

	//returns whether or not the shader requires recompilation (this can be deferred to the caller)
	bool SetDefine(std::string key, DefineType value, bool defer_recompilation = true);
	bool RemoveDefine(std::string key, bool defer_recompilation = true);

	template<typename T>
	T GetDefine(std::string key) const;

	template<typename T>
	static bool DefineTypesMatch(DefineType value);

	template<typename T>
	static T ConvertDefine(DefineType value);

	std::optional<std::string> CheckProgramValidity() const;
};