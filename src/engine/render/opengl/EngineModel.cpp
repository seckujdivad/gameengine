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
	
}