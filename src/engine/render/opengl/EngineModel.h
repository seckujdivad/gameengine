#pragma once

#include <wx/glcanvas.h>

#include <windows.data.json.h>
#include <fstream>
#include <vector>
#include <string>
#include <map>
#include <array>

class EngineModel
{
private:
	std::vector<std::vector<GLfloat>*>* m_vertices;
	std::vector<std::vector<int>*>* m_edges;
	std::vector<std::vector<int>*>* m_faces;

	std::vector<std::string>* m_fragment_shaders;

	std::map<std::string, int>* m_shaders; //shader name, count

	std::array<GLfloat, 3>* m_position;
	std::array<GLfloat, 3>* m_rotation;
	std::array<GLfloat, 3>* m_scale;

public:
	EngineModel();
	~EngineModel();

	void SetPosition(GLfloat x, GLfloat y, GLfloat z);
	void SetPosition(std::vector<GLfloat>* point);
	std::vector<GLfloat>* GetPosition();

	void SetRotation(GLfloat x, GLfloat y, GLfloat z);
	void SetRotation(std::vector<GLfloat>* rotation);
	std::vector<GLfloat>* GetRotation();

	void SetScale(GLfloat x, GLfloat y, GLfloat z);
	void SetScale(std::vector<GLfloat>* scale);
	std::vector<GLfloat>* GetScale();

	int AddVertex(GLfloat x, GLfloat y, GLfloat z);
	int AddVertex(std::vector<GLfloat>* vertex);
	bool RemoveVertex(int index);
	int FindVertex(GLfloat x, GLfloat y, GLfloat z);
	int FindVertex(std::vector<int>* vertex);
	std::vector<GLfloat>* GetVertex(int index);

	int AddEdge(std::vector<int>* vertex_indexes);
	bool RemoveEdge(int index);
	int FindEdge(std::vector<int>* vertex_indexes);
	std::vector<int>* GetEdge(int index);

	int AddFace(std::vector<int>* vertex_indices, std::string fragment_shader);
	bool RemoveFace(int index);
	int FindFace(std::vector<int>* vertex_indices);
	std::vector<int>* GetFace(int index);

	std::vector<std::vector<GLfloat>*>* GetTriFans();
};

