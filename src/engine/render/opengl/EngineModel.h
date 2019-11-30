#pragma once

#include <wx/glcanvas.h>

#include <windows.data.json.h>
#include <fstream>
#include <vector>
#include <string>

class EngineModel
{
private:
	std::vector<GLfloat*> m_vertices;
	std::vector<std::vector<int>> m_edges;
	std::vector<std::vector<int>> m_faces;
	std::vector<std::string> m_fragment_shaders;

public:
	EngineModel();
	~EngineModel();

	int AddVertex(GLfloat x, GLfloat y, GLfloat z);
	int AddVertex(GLfloat vertex[3]);
	bool RemoveVertex(int index);
	int GetIndex(GLfloat x, GLfloat y, GLfloat z);
	int GetIndex(GLfloat vertex[3]);

	int AddEdge(int* vertex_indexes);
	int AddEdge(std::vector<int> vertex_indexes);
	bool RemoveEdge(int index);
	int GetEdge(int* vertex_indexes);
	int GetEdge(std::vector<int> vertex_indexes);

	int AddFace(int* vertex_indexes, std::string fragment_shader);
	int AddFace(std::vector<int> vertex_indices, std::string fragment_shader);
	bool RemoveFace(int index);
	int GetFace(int* vertex_indeces);
	int GetFace(std::vector<int> vertex_indices);

	std::vector<std::vector<GLfloat>> GetTriFans();
};

