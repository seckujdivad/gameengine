#include "EngineModel.h"

EngineModel::EngineModel()
{
	this->m_vertices = new std::vector<std::vector<GLfloat>*>;
	this->m_edges = new std::vector<std::vector<int>*>;
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

void EngineModel::SetPosition(std::vector<GLfloat>* point)
{
}

std::vector<GLfloat>* EngineModel::GetPosition()
{
	return nullptr;
}

void EngineModel::SetRotation(GLfloat x, GLfloat y, GLfloat z)
{
}

void EngineModel::SetRotation(std::vector<GLfloat>* rotation)
{
}

std::vector<GLfloat>* EngineModel::GetRotation()
{
	return nullptr;
}

void EngineModel::SetScale(GLfloat x, GLfloat y, GLfloat z)
{
}

void EngineModel::SetScale(std::vector<GLfloat>* scale)
{
}

std::vector<GLfloat>* EngineModel::GetScale()
{
	return nullptr;
}

int EngineModel::AddVertex(GLfloat x, GLfloat y, GLfloat z)
{
	return 0;
}

int EngineModel::AddVertex(std::vector<GLfloat>* vertex)
{
	return 0;
}

bool EngineModel::RemoveVertex(int index)
{
	return false;
}

int EngineModel::FindVertex(GLfloat x, GLfloat y, GLfloat z)
{
	return 0;
}

int EngineModel::FindVertex(std::vector<int>* vertex)
{
	return 0;
}

std::vector<GLfloat>* EngineModel::GetVertex(int index)
{
	return nullptr;
}

int EngineModel::AddEdge(std::vector<int>* vertex_indexes)
{
	return 0;
}

bool EngineModel::RemoveEdge(int index)
{
	return false;
}

int EngineModel::FindEdge(std::vector<int>* vertex_indexes)
{
	return 0;
}

std::vector<int>* EngineModel::GetEdge(int index)
{
	return nullptr;
}

int EngineModel::AddFace(std::vector<int>* vertex_indices, std::string fragment_shader)
{
	return 0;
}

bool EngineModel::RemoveFace(int index)
{
	return false;
}

int EngineModel::FindFace(std::vector<int>* vertex_indices)
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
