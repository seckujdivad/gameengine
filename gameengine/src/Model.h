#pragma once

#include <wx/wxprec.h>
#include "GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <string>
#include <vector>
#include <map>
#include <array>
#include <tuple>
#include <string>

#include "Entity.h"
#include "render/ShaderProgram.h"

class Model : public Entity
{
private:
	std::vector<std::vector<GLfloat>*> m_vertices;
	std::vector<std::tuple<int, int>*> m_edges;
	std::vector<std::vector<int>*> m_faces;
	std::vector<std::vector<GLfloat>*> m_face_normals;

public:
	Model();
	Model(Model& copy_from);
	~Model();

	ShaderProgram shader_program;

	glm::mat4 position_rotate_matrix;
	glm::vec4 position_translate_vector;

	GLuint triangle_mode;
	GLuint vao = NULL;
	std::vector<GLuint> vertex_buffers;
	std::vector<GLint> vertex_buffers_count; //number of sets of vertices (6 floats means 2 sets)

	//vertices
	int AddVertex(GLfloat x, GLfloat y, GLfloat z);
	int AddVertex(std::vector<GLfloat> vertex);
	bool RemoveVertex(int index);
	bool RemoveVertex(size_t index);
	int FindVertex(GLfloat x, GLfloat y, GLfloat z);
	int FindVertex(std::vector<int> vertex);
	std::vector<GLfloat>* GetVertex(int index);
	std::vector<std::vector<GLfloat>> GetVerticesCopy();

	//edges
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

	//faces and face normals
	int AddFace(std::vector<int> edge_indexes, std::vector<GLfloat> normal); //edges must form a loop end-to-end in the given order
	bool RemoveFace(int index);
	bool RemoveFace(size_t index);
	int FindFace(std::vector<int> edge_indexes);
	std::vector<int>* GetFace(int index);
	std::vector<std::vector<int>> GetFacesCopy();
	std::vector<GLfloat>* GetFaceNormal(int index);

	std::vector<std::vector<GLfloat>> GetTriFans();
	std::vector<std::vector<GLfloat>*> GetTriStrips();

	int MergeVertices();
	int MergeVertices(GLfloat threshold);
	int MergeEdges();

	void GenPosMat();
	void GenVertexBuffer(GLuint triangle_mode);

	void RegisterUniforms();
	void SetUniforms();
};