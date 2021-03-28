#pragma once

#include <vector>

#include "../../GLComponents.h"
#include "../../scene/model/geometry/Geometry.h"

class GLGeometry
{
private:
	std::vector<double> m_data;

	GLsizei m_buffer_len = 0;
	GLuint m_vao = GL_NONE;
	GLuint m_vbo = GL_NONE;

	void CreateGLObjects();
	void Bind() const;

public:
	GLGeometry(std::vector<double> vertices, std::size_t primitive_size, Geometry::PrimitiveType primitive_type);
	GLGeometry(const GLGeometry&) = delete;
	GLGeometry& operator=(const GLGeometry&) = delete;
	GLGeometry(GLGeometry&& move_from) noexcept;
	GLGeometry& operator=(GLGeometry&& move_from) noexcept;
	~GLGeometry();

	//compare if the GLGeometry instances refer to the same OpenGL object
	bool operator==(const GLGeometry& other) const;
	bool operator!=(const GLGeometry& other) const;

	//compare the stored data to the given data
	bool operator==(const std::vector<double>& other) const;
	bool operator!=(const std::vector<double>& other) const;

	void SetData(std::vector<double> values, std::size_t primitive_size, Geometry::PrimitiveType primitive_type);

	void Draw(GLenum render_mode) const;
};