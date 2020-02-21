#include <wx/wxprec.h>
#include "Model.h"

Model::Model() : Entity()
{
	this->shader_program = new ShaderProgram(); //make black shader program so that proper errors are thrown
}

Model::Model(Model& copy_from) : Entity(copy_from)
{
	//copy geometry
	std::vector<std::vector<GLfloat>> vertices = copy_from.GetVerticesCopy();
	std::vector<GLfloat>* vertex_subvector;
	for (size_t i = 0; i < vertices.size(); i++)
	{
		vertex_subvector = new std::vector<GLfloat>;
		vertex_subvector->reserve(vertices.at(i).size());
		for (size_t j = 0; j < vertices.at(i).size(); j++)
		{
			vertex_subvector->push_back(vertices.at(i).at(j));
		}
		this->m_vertices.push_back(vertex_subvector);
	}

	std::vector<std::vector<int>> faces = copy_from.GetFacesCopy();
	std::vector<int>* faces_subvector;
	for (size_t i = 0; i < faces.size(); i++)
	{
		faces_subvector = new std::vector<int>;
		faces_subvector->reserve(faces.at(i).size());
		for (size_t j = 0; j < faces.at(i).size(); j++)
		{
			faces_subvector->push_back(faces.at(i).at(j));
		}
		this->m_faces.push_back(faces_subvector);

		this->m_face_normals.push_back(new glm::vec3(*copy_from.GetFaceNormal(i)));
	}

	this->shader_program = new ShaderProgram();
}

Model::~Model()
{
	for (size_t i = 0; i < this->m_vertices.size(); i++)
	{
		delete this->m_vertices.at(i);
	}

	for (size_t i = 0; i < this->m_faces.size(); i++)
	{
		delete this->m_faces.at(i);
		delete this->m_face_normals.at(i);
	}

	delete this->shader_program;
}

int Model::AddVertex(GLfloat x, GLfloat y, GLfloat z)
{
	std::vector<GLfloat>* vertex = new std::vector<GLfloat>;

	vertex->push_back(x);
	vertex->push_back(y);
	vertex->push_back(z);

	this->m_vertices.push_back(vertex);

	return this->m_vertices.size() - 1;
}

int Model::AddVertex(std::vector<GLfloat> vertex)
{
	return this->AddVertex(vertex.at(0), vertex.at(1), vertex.at(2));
}

bool Model::RemoveVertex(int index)
{
	return this->RemoveVertex((size_t)index);
}

bool Model::RemoveVertex(size_t index)
{
	if ((index >= 0) && (index < this->m_vertices.size()))
	{
		this->m_vertices.erase(this->m_vertices.begin() + index);
		return true;
	}
	else
	{
		return false;
	}
}

int Model::FindVertex(GLfloat x, GLfloat y, GLfloat z)
{
	std::vector<GLfloat>* current_item;

	for (size_t i = 0; i < this->m_vertices.size(); i++)
	{
		current_item = this->m_vertices.at(i);
		
		if ((current_item->at(0) == x) && (current_item->at(1) == y) && (current_item->at(2) == z))
		{
			return i;
		}
	}
	return -1;
}

int Model::FindVertex(std::vector<int> vertex)
{
	return this->FindVertex(vertex.at(0), vertex.at(1), vertex.at(2));
}

std::vector<GLfloat>* Model::GetVertex(int index)
{
	return this->m_vertices.at(index);
}

std::vector<std::vector<GLfloat>> Model::GetVerticesCopy()
{
	std::vector<std::vector<GLfloat>> output;
	std::vector<GLfloat> vertex;

	for (size_t i = 0; i < this->m_vertices.size(); i++)
	{
		vertex.clear();
		vertex.reserve(this->m_vertices.at(i)->size());
		for (size_t j = 0; j < this->m_vertices.at(i)->size(); j++)
		{
			vertex.push_back(this->m_vertices.at(i)->at(j));
		}
		output.push_back(vertex);
	}
	return output;
}

int Model::AddFace(std::vector<int> vertex_indices, glm::vec3 normal)
{
	//copy edge indices
	std::vector<int>* vertexes_copy = new std::vector<int>;

	for (size_t i = 0; i < vertex_indices.size(); i++)
	{
		vertexes_copy->push_back(vertex_indices.at(i));
	}

	this->m_faces.push_back(vertexes_copy);

	//copy normal
	this->m_face_normals.push_back(new glm::vec3(normal));

	//return face index
	return this->m_faces.size() - 1;
}

bool Model::RemoveFace(int index)
{
	return this->RemoveFace((size_t)index);
}

bool Model::RemoveFace(size_t index)
{
	if ((index >= 0) && (index < this->m_faces.size()))
	{
		this->m_faces.erase(this->m_faces.begin() + index);
		this->m_face_normals.erase(this->m_face_normals.begin() + index);
		return true;
	}
	else
	{
		return false;
	}
}

std::vector<int>* Model::GetFace(int index)
{
	return this->m_faces.at(index);
}

std::vector<std::vector<int>> Model::GetFacesCopy()
{
	std::vector<std::vector<int>> output;
	std::vector<int> face;
	output.reserve(this->m_faces.size());
	
	for (size_t i = 0; i < this->m_faces.size(); i++)
	{
		face.clear();
		face.reserve(this->m_faces.at(i)->size());
		for (size_t j = 0; j < this->m_faces.at(i)->size(); j++)
		{
			face.push_back(this->m_faces.at(i)->at(j));
		}
		output.push_back(face);
	}
	return output;
}

glm::vec3* Model::GetFaceNormal(int index)
{
	return this->m_face_normals.at(index);
}

std::vector<std::vector<GLfloat>> Model::GetTriFans()
{
	std::vector<std::vector<GLfloat>> trifans;
	std::vector<GLfloat> current_fan;
	std::vector<int>* current_face;

	for (size_t i = 0; i < this->m_faces.size(); i++)
	{
		current_face = this->m_faces.at(i);
		if (current_face->size() > 2)
		{
			current_fan.clear();
			for (size_t j = 0; j < this->m_faces.at(i)->size(); j++)
			{
				for (size_t k = 0; k < this->GetVertex(this->m_faces.at(i)->at(j))->size(); k++)
				{
					current_fan.push_back(this->GetVertex(this->m_faces.at(i)->at(j))->at(k));
				}
			}
			trifans.push_back(current_fan);
		}
	}
	return trifans;
}

std::vector<std::vector<GLfloat>*> Model::GetTriStrips()
{
	throw std::logic_error("Not implemented");
}

std::vector<GLfloat> Model::GetTriangles()
{
	std::vector<GLfloat> triangles;
	std::vector<GLfloat>* vertex;
	std::vector<int>* face_vertex_indices;

	std::vector<std::vector<int>> face_tri_verts;
	std::vector<glm::vec3> tri_vecs;

	glm::vec3 ccw_normal;
	glm::vec3 face_normal;
	float normal_angle_diff;

	for (size_t i = 0; i < this->m_faces.size(); i++)
	{
		face_vertex_indices = this->m_faces.at(i);
		if (face_vertex_indices->size() > 2) //lines aren't faces and shouldn't be rendered
		{
			face_tri_verts.clear();

			//get tris
			for (size_t j = 1; j < face_vertex_indices->size() - 1; j++)
			{
				face_tri_verts.push_back({ face_vertex_indices->at(0), face_vertex_indices->at(j), face_vertex_indices->at(j + 1) });
			}

			//get face normal
			face_normal = *this->m_face_normals.at(i);

			//unpack tri data
			for (size_t j = 0; j < face_tri_verts.size(); j++)
			{
				//get as vectors
				tri_vecs.clear();
				for (size_t k = 0; k < face_tri_verts.at(j).size(); k++)
				{
					vertex = this->GetVertex(face_tri_verts.at(j).at(k));
					tri_vecs.push_back(glm::vec3(vertex->at(0), vertex->at(1), vertex->at(2)));
				}

				//calculate ccw normal
				ccw_normal = glm::cross(tri_vecs.at(1) - tri_vecs.at(0), tri_vecs.at(2) - tri_vecs.at(0)); //follows right hand rule
				normal_angle_diff = std::abs(std::fmod(std::acos(glm::dot(ccw_normal, face_normal) / (glm::length(ccw_normal) * glm::length(face_normal))) + glm::pi<float>(), glm::pi<float>() * 2.0f) - glm::pi<float>());

				if (normal_angle_diff > (glm::pi<float>() / 2))
				{
					std::reverse(face_tri_verts.begin(), face_tri_verts.end());
				}

				//prep for array
				for (size_t k = 0; k < face_tri_verts.at(j).size(); k++)
				{
					vertex = this->GetVertex(face_tri_verts.at(j).at(k));
					for (size_t l = 0; l < vertex->size(); l++)
					{
						triangles.push_back(vertex->at(l));
					}
				}
			}
		}
	}

	return triangles;
}

int Model::MergeVertices()
{
	return this->MergeVertices(0.0f);
}

int Model::MergeVertices(GLfloat threshold)
{
	std::vector<int> merged_vertices;

	std::map<int, std::vector<int>> vertex_joins;
	std::map<int, std::vector<int>>::iterator vertex_match;
	
	//find duplicate vertices
	for (int i = 0; i < (int)this->m_vertices.size(); i++)
	{
		vertex_match = vertex_joins.end();
		for (std::map<int, std::vector<int>>::iterator it = vertex_joins.begin(); it != vertex_joins.end(); it++)
		{
			if (std::abs(this->m_vertices.at(it->first)->at(0) - this->m_vertices.at(i)->at(0)) < threshold
				&& std::abs(this->m_vertices.at(it->first)->at(1) - this->m_vertices.at(i)->at(1)) < threshold
				&& std::abs(this->m_vertices.at(it->first)->at(2) - this->m_vertices.at(i)->at(2)) < threshold)
			{
				vertex_match = it;
			}
		}

		if (vertex_match == vertex_joins.end())
		{
			vertex_joins.insert({ i, std::vector<int>() });
		}
		else
		{
			vertex_joins.at(vertex_match->first).push_back(i);
			merged_vertices.push_back(i);
		}
	}

	//change faces that use duplicated vertices to use the first vertex instance
	for (std::map<int, std::vector<int>>::iterator it = vertex_joins.begin(); it != vertex_joins.end(); it++)
	{
		for (int i = 0; i < (int)it->second.size(); i++)
		{
			for (size_t j = 0; j < this->m_faces.size(); j++)
			{
				for (size_t k = 0; k < this->m_faces.at(j)->size(); k++)
				{
					if (this->m_faces.at(j)->at(k) == it->second.at(i))
					{
						this->m_faces.at(j)->at(k) = it->first;
					}
				}
			}
		}
	}

	/*
	//remove unreferenced vertices
	for (int i = (int)merged_vertices.size() - 1; i >= 0; i--)
	{
		delete this->m_vertices.at(i);
		this->m_vertices.erase(this->m_vertices.begin() + i);
	}*/

	return (int)merged_vertices.size();
}

void Model::GenPosMat()
{
	this->position_rotate_matrix = glm::mat4(1.0f);
	this->position_rotate_matrix = glm::rotate(this->position_rotate_matrix, glm::radians(this->GetRotation(0)), glm::vec3(1.0f, 0.0f, 0.0f));
	this->position_rotate_matrix = glm::rotate(this->position_rotate_matrix, glm::radians(this->GetRotation(1)), glm::vec3(0.0f, 1.0f, 0.0f));
	this->position_rotate_matrix = glm::rotate(this->position_rotate_matrix, glm::radians(this->GetRotation(2)), glm::vec3(0.0f, 0.0f, 1.0f));

	this->position_scale_matrix = glm::mat4(1.0f);
	this->position_scale_matrix = glm::scale(this->position_rotate_matrix, glm::vec3(this->GetScale(0), this->GetScale(1), this->GetScale(2)));

	this->position_translate_vector = glm::vec4(this->GetPosition(0), this->GetPosition(1), this->GetPosition(2), 0.0f);
}

void Model::GenVertexBuffer(GLuint triangle_mode)
{
	//delete left over arrays and buffers
	if (this->vao != NULL)
	{
		glDeleteVertexArrays(1, &this->vao);
	}

	glGenVertexArrays(1, &this->vao);
	glBindVertexArray(this->vao);

	for (size_t i = 0; i < this->vertex_buffers.size(); i++)
	{
		glDeleteBuffers(1, &this->vertex_buffers.at(i));
	}
	this->vertex_buffers.clear();
	this->vertex_buffers_count.clear();

	//get verts in correct format
	this->triangle_mode = triangle_mode;

	if (triangle_mode == GL_TRIANGLE_FAN)
	{
		std::vector<std::vector<GLfloat>> trifans = this->GetTriFans();

		std::vector<GLfloat> fan_data;

		for (size_t i = 0; i < 1; i++)//trifans.size(); i++)
		{
			fan_data.clear();
			for (size_t j = 0; j < trifans.at(i).size(); j++)
			{
				for (size_t k = 0; k < this->m_vertices.at(trifans.at(i).at(j))->size(); k++)
				{
					fan_data.push_back(this->m_vertices.at(trifans.at(i).at(j))->at(k));
				}
			}

			GLuint vertex_buffer;
			glGenBuffers(1, &vertex_buffer);
			glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
			glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * fan_data.size(), fan_data.data(), GL_STATIC_DRAW);
			glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), 0);
			glEnableVertexAttribArray(0);

			this->vertex_buffers.push_back(vertex_buffer);
			this->vertex_buffers_count.push_back(trifans.at(i).size() / 3);
		}
		
	}
	else if (triangle_mode == GL_TRIANGLES)
	{
		std::vector<GLfloat> triangles = this->GetTriangles();

		GLfloat* tris = triangles.data();
		int size = triangles.size();

		GLuint vertex_buffer;
		glGenBuffers(1, &vertex_buffer);
		glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
		glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * triangles.size(), triangles.data(), GL_STATIC_DRAW);
		this->vertex_buffers.push_back(vertex_buffer);
		this->vertex_buffers_count.push_back(triangles.size() / 3);

		glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), 0);
		glEnableVertexAttribArray(0);
	}
}

void Model::RegisterUniforms()
{
	this->shader_program->RegisterUniform("mdl_rotate");
	this->shader_program->RegisterUniform("mdl_translate");
	this->shader_program->RegisterUniform("mdl_scale");
}

void Model::SetUniforms()
{
	glUniformMatrix4fv(this->shader_program->GetUniform("mdl_rotate"), 1, GL_FALSE, glm::value_ptr(this->position_rotate_matrix));
	glUniformMatrix4fv(this->shader_program->GetUniform("mdl_scale"), 1, GL_FALSE, glm::value_ptr(this->position_scale_matrix));
	glUniform4fv(this->shader_program->GetUniform("mdl_translate"), 1, glm::value_ptr(this->position_translate_vector));
}