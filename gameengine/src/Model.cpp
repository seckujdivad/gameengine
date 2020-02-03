#include <wx/wxprec.h>
#include "Model.h"

Model::Model() : Entity()
{

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

	std::vector<std::tuple<int, int>> edges = copy_from.GetEdgesCopy();
	std::tuple<int, int>* edge_tuple;
	for (size_t i = 0; i < edges.size(); i++)
	{
		edge_tuple = new std::tuple<int, int>;
		std::get<0>(*edge_tuple) = std::get<0>(edges.at(i));
		std::get<1>(*edge_tuple) = std::get<1>(edges.at(i));
		this->m_edges.push_back(edge_tuple);
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
	}
}

Model::~Model()
{
	for (size_t i = 0; i < this->m_vertices.size(); i++)
	{
		delete this->m_vertices.at(i);
	}

	for (size_t i = 0; i < this->m_edges.size(); i++)
	{
		delete this->m_edges.at(i);
	}

	for (size_t i = 0; i < this->m_faces.size(); i++)
	{
		delete this->m_faces.at(i);
	}
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

int Model::AddEdge(int index0, int index1)
{
	std::tuple<int, int>* vertex_indexes = new std::tuple<int, int>(index0, index1);
	this->m_edges.push_back(vertex_indexes);
	return this->m_edges.size() - 1;
}

int Model::AddEdge(std::vector<int> vertex_indexes)
{
	if (vertex_indexes.size() == 2)
	{
		return this->AddEdge(vertex_indexes.at(0), vertex_indexes.at(1));
	}
	else
	{
		throw std::length_error("'vertex_indexes' must contain exactly 2 items");
	}
}

int Model::AddEdge(std::tuple<int, int> vertex_indexes)
{
	return this->AddEdge(std::get<0>(vertex_indexes), std::get<1>(vertex_indexes));
}

bool Model::RemoveEdge(int index)
{
	return this->RemoveEdge((size_t)index);
}

bool Model::RemoveEdge(size_t index)
{
	if ((0 < index) || (index >= this->m_edges.size()))
	{
		return false;
	}
	else
	{
		this->m_edges.erase(this->m_edges.begin() + index);
		return true;
	}
}

int Model::FindEdge(int index0, int index1)
{
	std::tuple<int, int>* current_edge;
	for (size_t i = 0; i < this->m_edges.size(); i++)
	{
		current_edge = this->m_edges.at(i);
		if (((std::get<0>(*current_edge) == index0) && (std::get<1>(*current_edge) == index1))
			|| ((std::get<1>(*current_edge) == index0) && (std::get<0>(*current_edge) == index1)))
		{
			return i;
		}
	}
	return -1;
}

int Model::FindEdge(std::vector<int> vertex_indexes)
{
	if (vertex_indexes.size() == 2)
	{
		return this->FindEdge(vertex_indexes.at(0), vertex_indexes.at(1));
	}
	else
	{
		throw std::length_error("'vertex_indexes' must contain exactly 2 items");
	}
}

int Model::FindEdge(std::tuple<int, int> vertex_indexes)
{
	return this->FindEdge(std::get<0>(vertex_indexes), std::get<1>(vertex_indexes));
}

std::tuple<int, int>* Model::GetEdge(int index)
{
	return this->GetEdge((size_t)index);
}

std::tuple<int, int>* Model::GetEdge(size_t index)
{
	if ((index >= 0) && (index < this->m_edges.size()))
	{
		return this->m_edges.at(index);
	}
	else
	{
		throw std::range_error("index doesn't point to an edge");
	}
}

std::vector<int> Model::GetEdgeVec(int index)
{
	std::tuple<int, int> edge = *this->GetEdge(index);
	std::vector<int> vector;
	vector.push_back(std::get<0>(edge));
	vector.push_back(std::get<0>(edge));
	return vector;
}

std::vector<std::tuple<int, int>> Model::GetEdgesCopy()
{
	std::vector<std::tuple<int, int>> output;
	std::tuple<int, int> vertex_pair;

	for (size_t i = 0; i < this->m_edges.size(); i++)
	{
		std::get<0>(vertex_pair) = std::get<0>(*this->m_edges.at(i));
		std::get<1>(vertex_pair) = std::get<1>(*this->m_edges.at(i));
		output.push_back(vertex_pair);
	}
	return output;
}

int Model::AddFace(std::vector<int> edge_indexes, std::string fragment_shader)
{
	std::vector<int>* edges_copy = new std::vector<int>;

	for (size_t i = 0; i < edge_indexes.size(); i++)
	{
		edges_copy->push_back(edge_indexes.at(i));
	}
	std::sort(edges_copy->begin(), edges_copy->end());

	this->m_faces.push_back(edges_copy);

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
		return true;
	}
	else
	{
		return false;
	}
}

int Model::FindFace(std::vector<int> edge_indexes)
{
	std::sort(edge_indexes.begin(), edge_indexes.end());

	bool is_match;
	
	for (size_t i = 0; i < this->m_faces.size(); i++)
	{
		if (this->m_faces.at(i)->size() == edge_indexes.size())
		{
			is_match = true;
			for (size_t j = 0; j < edge_indexes.size(); j++)
			{
				if (is_match)
				{
					if (edge_indexes.at(j) != this->m_faces.at(i)->at(j))
					{
						is_match = false;
					}
				}
			}
			if (is_match)
			{
				return i;
			}
		}
	}
	return -1;
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

std::vector<std::vector<GLfloat>> Model::GetTriFans()
{
	std::vector<std::vector<GLfloat>> trifans;
	std::vector<GLfloat> current_fan;
	std::vector<int>* current_face;

	for (size_t i = 0; i < this->m_faces.size(); i++)
	{
		current_face = this->m_faces.at(i);
		if (current_face->size() > 0)
		{
			current_fan.clear();
			for (size_t j = 0; j < current_face->size(); j++)
			{
				current_fan.push_back(std::get<0>(*this->m_edges.at(current_face->at(j))));
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

int Model::MergeVertices()
{
	return this->MergeVertices(0.0f);
}

int Model::MergeVertices(GLfloat threshold)
{
	std::vector<int> merged_vertices;
	std::tuple<int, int>* current_edge;

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

	//change edges that use duplicated vertices to use the first vertex instance
	for (std::map<int, std::vector<int>>::iterator it = vertex_joins.begin(); it != vertex_joins.end(); it++)
	{
		for (int i = 0; i < (int)it->second.size(); i++)
		{
			for (size_t j = 0; j < this->m_edges.size(); j++)
			{
				current_edge = nullptr;
				if (std::get<0>(*this->m_edges.at(j)) == it->second.at(i)) //can't use run-time tuple indexing
				{
					current_edge = new std::tuple<int, int>(it->first, std::get<1>(*this->m_edges.at(j)));
				}
				else if (std::get<1>(*this->m_edges.at(j)) == it->second.at(i))
				{
					current_edge = new std::tuple<int, int>(std::get<0>(*this->m_edges.at(j)), it->first);
				}

				if (current_edge != nullptr)
				{
					delete this->m_edges.at(j);
					this->m_edges.at(j) = current_edge;
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

int Model::MergeEdges()
{
	std::vector<int> merged_edges;
	std::map<int, std::vector<int>> edge_joins;
	std::map<int, std::vector<int>>::iterator edges_match;

	//find duplicate edges
	for (size_t i = 0; i < this->m_edges.size(); i++)
	{
		edges_match = edge_joins.end();
		for (std::map<int, std::vector<int>>::iterator it = edge_joins.begin(); it != edge_joins.end(); it++)
		{
			if ((std::get<0>(*this->m_edges.at(it->first)) == std::get<0>(*this->m_edges.at(i))
				&& std::get<1>(*this->m_edges.at(it->first)) == std::get<1>(*this->m_edges.at(i)))
				|| (std::get<0>(*this->m_edges.at(it->first)) == std::get<1>(*this->m_edges.at(i))
					&& std::get<1>(*this->m_edges.at(it->first)) == std::get<0>(*this->m_edges.at(i))))
			{
				edges_match = it;
			}
		}

		if (edges_match == edge_joins.end())
		{
			edge_joins.insert({ i, std::vector<int>() });
		}
		else
		{
			edge_joins.at(edges_match->first).push_back(i);
			merged_edges.push_back(i);
		}
	}

	//change faces that use duplicate edges to use first instance of that edge
	for (std::map<int, std::vector<int>>::iterator it = edge_joins.begin(); it != edge_joins.end(); it++)
	{
		for (size_t i = 0; i < it->second.size(); i++)
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
	//remove unreferenced edges
	for (int i = (int)merged_edges.size() - 1; i >= 0; i--)
	{
		delete this->m_edges.at(i);
		this->m_edges.erase(this->m_edges.begin() + i);
	}*/

	return (int)merged_edges.size();
}

void Model::GenPosMat()
{
	this->position_rotate_matrix = glm::mat4(1.0f);
	this->position_rotate_matrix = glm::scale(this->position_rotate_matrix, glm::vec3(this->GetScale(0), this->GetScale(1), this->GetScale(2)));
	this->position_rotate_matrix = glm::rotate(this->position_rotate_matrix, glm::radians(this->GetRotation(0)), glm::vec3(1.0f, 0.0f, 0.0f));
	this->position_rotate_matrix = glm::rotate(this->position_rotate_matrix, glm::radians(this->GetRotation(1)), glm::vec3(0.0f, 1.0f, 0.0f));
	this->position_rotate_matrix = glm::rotate(this->position_rotate_matrix, glm::radians(this->GetRotation(2)), glm::vec3(0.0f, 0.0f, 1.0f));

	this->position_translate_vector = glm::vec4(this->GetPosition(0), this->GetPosition(1), this->GetPosition(2), 0.0f);
}

void Model::GenVertexBuffer(GLuint triangle_mode)
{
	//delete left over arrays and buffers
	if (this->vao == NULL)
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

		for (size_t i = 0; i < trifans.size(); i++)
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
			glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * trifans.at(i).size(), fan_data.data(), GL_STATIC_DRAW);
			this->vertex_buffers.push_back(vertex_buffer);
			this->vertex_buffers_count.push_back(trifans.at(i).size());

			glVertexAttribPointer(i, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), 0);
			glEnableVertexAttribArray(i);
		}
	}
	else if (triangle_mode == GL_TRIANGLES)
	{
		std::vector<GLfloat> triangles;
		std::vector<std::vector<GLfloat>> trifans = this->GetTriFans();
		for (size_t i = 0; i < trifans.size(); i++)
		{
			for (size_t j = 2; j < trifans.at(i).size(); j++)
			{
				for (size_t k = 0; k < 3; k++)
				{
					triangles.push_back(this->m_vertices.at(trifans.at(i).at(0))->at(k));
				}

				for (size_t k = 0; k < 3; k++)
				{
					triangles.push_back(this->m_vertices.at(trifans.at(i).at(j - 1))->at(k));
				}

				for (size_t k = 0; k < 3; k++)
				{
					triangles.push_back(this->m_vertices.at(trifans.at(i).at(j))->at(k));
				}
			}
		}

		GLuint vertex_buffer;
		glGenBuffers(1, &vertex_buffer);
		glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
		glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * triangles.size(), triangles.data(), GL_STATIC_DRAW);
		this->vertex_buffers.push_back(vertex_buffer);
		this->vertex_buffers_count.push_back(triangles.size());

		glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), 0);
		glEnableVertexAttribArray(0);
	}
}

void Model::RegisterUniforms()
{
	this->shader_program.RegisterUniform("mdl_rotate");
	this->shader_program.RegisterUniform("mdl_translate");
}

void Model::SetUniforms()
{
	glUniformMatrix4fv(this->shader_program.GetUniform("mdl_rotate"), 1, GL_FALSE, glm::value_ptr(this->position_rotate_matrix));
	glUniform4fv(this->shader_program.GetUniform("mdl_translate"), 1, glm::value_ptr(this->position_translate_vector));
}