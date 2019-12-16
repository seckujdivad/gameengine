#pragma once

#include <wx/glcanvas.h>

#include <string>
#include <vector>
#include <map>
#include <array>
#include <tuple>
#include <string>

class Model
{
private:
	std::vector<std::vector<GLfloat>*>* m_vertices;
	std::vector<std::tuple<int, int>*>* m_edges;
	std::vector<std::vector<int>*>* m_faces;

	std::array<GLfloat, 3>* m_position;
	std::array<GLfloat, 3>* m_rotation;
	std::array<GLfloat, 3>* m_scale;

	std::string m_identifier;

public:
	Model();
	Model(Model* copy_from);
	~Model();

	void SetIdentifier(std::string identifier);
	std::string GetIdentifier();

	void SetPosition(GLfloat x, GLfloat y, GLfloat z);
	void SetPosition(std::vector<GLfloat> point);
	std::vector<GLfloat> GetPosition();

	void SetRotation(GLfloat x, GLfloat y, GLfloat z);
	void SetRotation(std::vector<GLfloat> rotation);
	std::vector<GLfloat> GetRotation();

	void SetScale(GLfloat x, GLfloat y, GLfloat z);
	void SetScale(std::vector<GLfloat> scale);
	std::vector<GLfloat> GetScale();

	int AddVertex(GLfloat x, GLfloat y, GLfloat z);
	int AddVertex(std::vector<GLfloat> vertex);
	bool RemoveVertex(int index);
	bool RemoveVertex(size_t index);
	int FindVertex(GLfloat x, GLfloat y, GLfloat z);
	int FindVertex(std::vector<int> vertex);
	std::vector<GLfloat>* GetVertex(int index);
	std::vector<std::vector<GLfloat>> GetVerticesCopy();

	int AddEdge(std::vector<int> vertex_indexes);
	int AddEdge(int index0, int index1);
	int AddEdge(std::tuple<int, int> vertex_indexes);
	bool RemoveEdge(int index);
	bool RemoveEdge(size_t index);
	int FindEdge(std::vector<int> vertex_indexes);
	int FindEdge(int index0, int index1);
	int FindEdge(std::tuple<int, int> vertex_indexes);
	std::tuple<int, int>* GetEdge(int index);
	std::tuple<int, int>* GetEdge(size_t index);
	std::vector<int> GetEdgeVec(int index);
	std::vector<std::tuple<int, int>> GetEdgesCopy();

	int AddFace(std::vector<int> edge_indexes, std::string fragment_shader); //edges must form a loop end-to-end in the given order
	bool RemoveFace(int index);
	bool RemoveFace(size_t index);
	int FindFace(std::vector<int> edge_indexes);
	std::vector<int>* GetFace(int index);
	std::vector<std::vector<int>> GetFacesCopy();

	std::vector<std::vector<GLfloat>> GetTriFans();
	std::vector<std::vector<GLfloat>*> GetTriStrips();
};