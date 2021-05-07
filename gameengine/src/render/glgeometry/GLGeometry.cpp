#include "GLGeometry.h"

#include <stdexcept>

#include "../../PatchSize.h"

void GLGeometry::SetData(std::vector<double> vertices, Geometry::RenderInfo render_info)
{
	this->m_data = vertices;
	this->m_render_info = render_info;

	std::vector<GLfloat> padded_vertices;

	if (this->m_render_info.primitive_size > GAMEENGINE_PATCH_SIZE)
	{
		throw std::invalid_argument("Primitives must have less than " + std::to_string(GAMEENGINE_PATCH_SIZE) + " vertices");
	}

#ifdef _DEBUG
	if (this->m_data.size() % static_cast<std::size_t>(GAMEENGINE_VALUES_PER_VERTEX) != 0)
	{
		throw std::invalid_argument("Incomplete vertices provided");
	}
#endif

	if ((this->m_render_info.primitive_type == Geometry::PrimitiveType::Patches || this->m_render_info.primitive_type == Geometry::PrimitiveType::Quads)
		&& this->m_render_info.primitive_size != GAMEENGINE_PATCH_SIZE)
	{
		std::size_t num_primitives = vertices.size() / (this->m_render_info.primitive_size * GAMEENGINE_VALUES_PER_VERTEX);
		padded_vertices.reserve(num_primitives * GAMEENGINE_PATCH_SIZE * GAMEENGINE_VALUES_PER_VERTEX);
		for (std::size_t i = 0; i < num_primitives; i++)
		{
			for (std::size_t j = 0; j < this->m_render_info.primitive_size * GAMEENGINE_VALUES_PER_VERTEX; j++)
			{
				padded_vertices.push_back(static_cast<GLfloat>(vertices.at((i * this->m_render_info.primitive_size * GAMEENGINE_VALUES_PER_VERTEX) + j)));
			}

			for (std::size_t j = 0; j < GAMEENGINE_PATCH_SIZE - this->m_render_info.primitive_size; j++)
			{
				for (std::size_t k = 0; k < std::size_t(GAMEENGINE_VALUES_PER_VERTEX); k++)
				{
					padded_vertices.push_back(0.0f);
				}
			}
		}
	}
	else
	{
		padded_vertices.reserve(vertices.size());
		for (double value : vertices)
		{
			padded_vertices.push_back(static_cast<GLfloat>(value));
		}
	}

	this->m_buffer_len = static_cast<GLsizei>(padded_vertices.size());

#ifdef _DEBUG
	if (this->m_buffer_len % static_cast<std::size_t>(GAMEENGINE_VALUES_PER_VERTEX) != 0)
	{
		throw std::runtime_error("Incomplete vertices generated");
	}
#endif

	this->Bind();
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * padded_vertices.size(), padded_vertices.data(), GL_DYNAMIC_DRAW);

#ifdef _DEBUG
	glBindVertexArray(NULL);
	glBindBuffer(GL_ARRAY_BUFFER, NULL);
#endif
}

void GLGeometry::CreateGLObjects()
{
	glGenVertexArrays(1, &this->m_vao);
	glBindVertexArray(this->m_vao);

	glGenBuffers(1, &this->m_vbo);
	glBindBuffer(GL_ARRAY_BUFFER, this->m_vbo);

	this->SetName(this->m_vao);

	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, GAMEENGINE_VALUES_PER_VERTEX * sizeof(GLfloat), 0);
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, GAMEENGINE_VALUES_PER_VERTEX * sizeof(GLfloat), (void*)(3 * sizeof(GLfloat)));
	glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, GAMEENGINE_VALUES_PER_VERTEX * sizeof(GLfloat), (void*)(6 * sizeof(GLfloat)));

	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glEnableVertexAttribArray(2);
}

void GLGeometry::Bind() const
{
	glBindVertexArray(this->m_vao);
	glBindBuffer(GL_ARRAY_BUFFER, this->m_vbo);
}

GLGeometry::GLGeometry(std::vector<double> vertices, Geometry::RenderInfo render_info) : GLObjectLabelable(GL_VERTEX_ARRAY)
{
	this->CreateGLObjects();
	this->SetData(vertices, render_info);
}

GLGeometry::GLGeometry(GLGeometry&& move_from) noexcept : GLObjectLabelable(GL_VERTEX_ARRAY)
{
	*this = std::move(move_from);
}

GLGeometry& GLGeometry::operator=(GLGeometry&& move_from) noexcept
{
	this->m_vao = move_from.m_vao;
	move_from.m_vao = GL_NONE;
	this->m_vbo = move_from.m_vbo;
	move_from.m_vbo = GL_NONE;

	this->m_buffer_len = move_from.m_buffer_len;

	this->m_data = move_from.m_data;
	this->m_render_info = move_from.m_render_info;

	this->SetName(this->m_vao);

	return *this;
}

GLGeometry::~GLGeometry()
{
	glDeleteBuffers(1, &this->m_vbo);
	glDeleteVertexArrays(1, &this->m_vao);
}

bool GLGeometry::operator==(const GLGeometry& other) const //no need to compare m_data as that is not relevant to the comparison specified in the header
{
	if (this->m_vao != other.m_vao)
	{
		return false;
	}

	if (this->m_vbo != other.m_vbo)
	{
		return false;
	}

	return true;
}

bool GLGeometry::operator!=(const GLGeometry& other) const
{
	return !(*this == other);
}

bool GLGeometry::operator==(const std::vector<double>& other) const
{
	return this->m_data == other;
}

bool GLGeometry::operator!=(const std::vector<double>& other) const
{
	return this->m_data != other;
}

const std::vector<double>& GLGeometry::GetValues() const
{
	return this->m_data;
}

const Geometry::RenderInfo& GLGeometry::GetRenderInfo() const
{
	return this->m_render_info;
}

void GLGeometry::Draw() const
{
	this->Bind();

	GLsizei num_elements = static_cast<GLsizei>(this->m_buffer_len / static_cast<std::size_t>(GAMEENGINE_VALUES_PER_VERTEX));
	glDrawArrays(GetPrimitiveTypeRenderMode(this->m_render_info.primitive_type), 0, num_elements);

	glBindVertexArray(GL_NONE);
	glBindBuffer(GL_ARRAY_BUFFER, GL_NONE);
}

GLenum GetPrimitiveTypeRenderMode(Geometry::PrimitiveType primitive_type)
{
	switch (primitive_type)
	{
	case Geometry::PrimitiveType::Patches:
	case Geometry::PrimitiveType::Quads:
		return GL_PATCHES;
	case Geometry::PrimitiveType::Triangles:
		return GL_TRIANGLES;
	default: throw std::invalid_argument("Unknown primitive type " + std::to_string(static_cast<int>(primitive_type)));
	}
}
