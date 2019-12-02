#include "EngineModel.h"

EngineModel::EngineModel()
{
	this->m_vertices = new std::vector<std::vector<GLfloat>*>;
	this->m_edges = new std::vector<std::tuple<int, int>*>;
	this->m_faces = new std::vector<std::vector<int>*>;

	this->m_fragment_shaders = new std::vector<std::string>;

	this->m_shaders = new std::map<std::string, int>;

	this->m_position = new std::array<GLfloat, 3> {0, 0, 0};
	this->m_rotation = new std::array<GLfloat, 3> {0, 0, 0};
	this->m_scale = new std::array<GLfloat, 3> {0, 0, 0};
}

EngineModel::~EngineModel()
{
	for (int i = 0; i < this->m_vertices->size(); i++)
	{
		delete this->m_vertices->at(i);
	}
	delete this->m_vertices;

	for (int i = 0; i < this->m_edges->size(); i++)
	{
		delete this->m_edges->at(i);
	}
	delete this->m_edges;

	for (int i = 0; i < this->m_faces->size(); i++)
	{
		delete this->m_faces->at(i);
	}
	delete this->m_faces;

	delete this->m_fragment_shaders;
	delete this->m_shaders;
	delete this->m_position;
	delete this->m_rotation;
	delete this->m_scale;
}

void EngineModel::SetPosition(GLfloat x, GLfloat y, GLfloat z)
{
	this->m_position->at(0) = x;
	this->m_position->at(1) = y;
	this->m_position->at(2) = z;
}

void EngineModel::SetPosition(std::vector<GLfloat> point)
{
	if (point.size() == 3)
	{
		for (int i = 0; i < 3; i++)
		{
			this->m_position->at(i) = point.at(i);
		}
	}
	else
	{
		throw std::length_error("'point' must contain exactly 3 items");
	}
}

std::vector<GLfloat> EngineModel::GetPosition()
{
	std::vector<GLfloat> result;
	for (int i = 0; i < 3; i++)
	{
		result.push_back(this->m_position->at(i));
	}
	return result;
}

void EngineModel::SetRotation(GLfloat x, GLfloat y, GLfloat z)
{
	this->m_rotation->at(0) = x;
	this->m_rotation->at(1) = y;
	this->m_rotation->at(2) = z;
}

void EngineModel::SetRotation(std::vector<GLfloat> rotation)
{
	if (rotation.size() == 3)
	{
		for (int i = 0; i < 3; i++)
		{
			this->m_rotation->at(i) = rotation.at(i);
		}
	}
	else
	{
		throw std::length_error("'rotation' must contain exactly 3 items");
	}
}

std::vector<GLfloat> EngineModel::GetRotation()
{
	std::vector<GLfloat> result;
	for (int i = 0; i < 3; i++)
	{
		result.push_back(this->m_rotation->at(i));
	}
	return result;
}

void EngineModel::SetScale(GLfloat x, GLfloat y, GLfloat z)
{
	this->m_scale->at(0) = x;
	this->m_scale->at(1) = y;
	this->m_scale->at(2) = z;
}

void EngineModel::SetScale(std::vector<GLfloat> scale)
{
	if (scale.size() == 3)
	{
		for (int i = 0; i < 3; i++)
		{
			this->m_scale->at(i) = scale.at(i);
		}
	}
	else
	{
		throw std::length_error("'scale' must contain exactly 3 items");
	}
}

std::vector<GLfloat> EngineModel::GetScale()
{
	std::vector<GLfloat> result;
	for (int i = 0; i < 3; i++)
	{
		result.push_back(this->m_scale->at(i));
	}
	return result;
}

int EngineModel::AddVertex(GLfloat x, GLfloat y, GLfloat z)
{
	std::vector<GLfloat>* vertex = new std::vector<GLfloat>;

	vertex->push_back(x);
	vertex->push_back(y);
	vertex->push_back(z);

	this->m_vertices->push_back(vertex);

	return this->m_vertices->size() - 1;
}

int EngineModel::AddVertex(std::vector<GLfloat> vertex)
{
	return this->AddVertex(vertex.at(0), vertex.at(1), vertex.at(2));
}

bool EngineModel::RemoveVertex(int index)
{
	if ((index >= 0) && (index < this->m_vertices->size()))
	{
		this->m_vertices->erase(this->m_vertices->begin() + index);
		return true;
	}
	else
	{
		return false;
	}
}

int EngineModel::FindVertex(GLfloat x, GLfloat y, GLfloat z)
{
	std::vector<GLfloat>* current_item;

	for (int i = 0; i < this->m_vertices->size(); i++)
	{
		current_item = this->m_vertices->at(i);
		
		if ((current_item->at(0) == x) && (current_item->at(1) == y) && (current_item->at(2) == z))
		{
			return i;
		}
	}
	return -1;
}

int EngineModel::FindVertex(std::vector<int> vertex)
{
	return this->FindVertex(vertex.at(0), vertex.at(1), vertex.at(2));
}

std::vector<GLfloat>* EngineModel::GetVertex(int index)
{
	return this->m_vertices->at(index);
}

int EngineModel::AddEdge(int index0, int index1)
{
	std::tuple<int, int>* vertex_indexes = &std::make_tuple(index0, index1);
	this->m_edges->push_back(vertex_indexes);
	return this->m_edges->size() - 1;
}

int EngineModel::AddEdge(std::vector<int> vertex_indexes)
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

int EngineModel::AddEdge(std::tuple<int, int> vertex_indexes)
{
	return this->AddEdge(std::get<0>(vertex_indexes), std::get<1>(vertex_indexes));
}

bool EngineModel::RemoveEdge(int index)
{
	if ((0 < index) || (index >= this->m_edges->size()))
	{
		return false;
	}
	else
	{
		this->m_edges->erase(this->m_edges->begin() + index);
		return true;
	}
}

int EngineModel::FindEdge(int index0, int index1)
{
	std::tuple<int, int>* current_edge;
	for (int i = 0; i < this->m_edges->size(); i++)
	{
		current_edge = this->m_edges->at(i);
		if (((std::get<0>(*current_edge) == index0) && (std::get<1>(*current_edge) == index1))
			|| ((std::get<1>(*current_edge) == index0) && (std::get<0>(*current_edge) == index1)))
		{
			return i;
		}
	}
	return -1;
}

int EngineModel::FindEdge(std::vector<int> vertex_indexes)
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

int EngineModel::FindEdge(std::tuple<int, int> vertex_indexes)
{
	return this->FindEdge(std::get<0>(vertex_indexes), std::get<1>(vertex_indexes));
}

std::tuple<int, int>* EngineModel::GetEdge(int index)
{
	if ((index >= 0) && (index < this->m_edges->size()))
	{
		return this->m_edges->at(index);
	}
	else
	{
		throw std::range_error("index doesn't point to an edge");
	}
}

std::vector<int> EngineModel::GetEdgeVec(int index)
{
	std::tuple<int, int> edge = *this->GetEdge(index);
	std::vector<int> vector;
	vector.push_back(std::get<0>(edge));
	vector.push_back(std::get<0>(edge));
	return vector;
}

int EngineModel::AddFace(std::vector<int> edge_indexes, std::string fragment_shader)
{
	std::vector<int>* edges_copy = new std::vector<int>;

	for (int i = 0; i < edge_indexes.size(); i++)
	{
		edges_copy->push_back(edge_indexes.at(i));
	}

	this->m_faces->push_back(edges_copy);

	return this->m_faces->size() - 1;
}

bool EngineModel::RemoveFace(int index)
{
	if ((index >= 0) && (index < this->m_faces->size()))
	{
		this->m_faces->erase(this->m_faces->begin() + index);
		return true;
	}
	else
	{
		return false;
	}
}

int EngineModel::FindFace(std::vector<int> edge_indexes)
{
	return 0;
}

std::vector<int>* EngineModel::GetFace(int index)
{
	return nullptr;
}

std::vector<std::vector<std::vector<GLfloat>*>*>* EngineModel::GetTriFans()
{
	return nullptr;
}

std::vector<std::vector<std::vector<GLfloat>*>*>* EngineModel::GetTriStrips()
{
	return nullptr;
}
