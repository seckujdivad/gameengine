#pragma once

#include <vector>

#include "../../GLComponents.h"
#include "../../scene/model/geometry/Geometry.h"

class GLGeometry
{
private:
	std::vector<GLfloat> m_data;

	GLsizei m_buffer_len = 0;
	GLuint m_vao = GL_NONE;
	GLuint m_vbo = GL_NONE;

	void CreateGLObjects();
	void Bind() const;

public:
	template<typename T, typename = std::enable_if_t<std::is_convertible_v<T, GLfloat>>>
	inline GLGeometry(std::vector<T> vertices, std::size_t primitive_size, Geometry::PrimitiveType primitive_type)
	{
		this->CreateGLObjects();
		this->SetData(vertices, primitive_size, primitive_type);
	};

	GLGeometry(const GLGeometry&) = delete;
	GLGeometry& operator=(const GLGeometry&) = delete;
	GLGeometry(GLGeometry&& move_from) noexcept;
	GLGeometry& operator=(GLGeometry&& move_from) noexcept;
	~GLGeometry();

	//compare if the GLGeometry instances refer to the same OpenGL object
	bool operator==(const GLGeometry& other) const;
	bool operator!=(const GLGeometry& other) const;

	//compare the stored data to the given data
	template<typename T, typename = std::enable_if_t<std::is_convertible_v<T, GLfloat>>>
	inline bool operator==(const std::vector<T>& other)
	{
		if (this->m_data.size() == other.size())
		{
			for (std::size_t i = 0; i < this->m_data.size(); i++)
			{
				if (this->m_data.at(i) != static_cast<GLfloat>(other.at(i)))
				{
					return false;
				}
			}
			return true;
		}
		else
		{
			return false;
		}
	}

	template<typename T, typename = std::enable_if_t<std::is_convertible_v<T, GLfloat>>>
	inline bool operator!=(const std::vector<T>& other)
	{
		return !(*this == other);
	}
	
	template<typename T, typename = std::enable_if_t<std::is_convertible_v<T, GLfloat>>>
	inline void SetData(std::vector<T> values, std::size_t primitive_size, Geometry::PrimitiveType primitive_type)
	{
		std::vector<GLfloat> values_converted;
		values_converted.reserve(values.size());
		for (T value : values)
		{
			values_converted.push_back(static_cast<GLfloat>(value));
		}
		this->SetData(values_converted, primitive_size, primitive_type);
	}
	void SetData(std::vector<GLfloat> values, std::size_t primitive_size, Geometry::PrimitiveType primitive_type);

	void Draw(GLenum render_mode) const;
};