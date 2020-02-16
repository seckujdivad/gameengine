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
	std::vector<std::vector<int>*> m_faces;
	std::vector<glm::vec3*> m_face_normals;

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

	//faces and face normals
	int AddFace(std::vector<int> vertex_indices, glm::vec3 normal);
	bool RemoveFace(int index);
	bool RemoveFace(size_t index);
	std::vector<int>* GetFace(int index);
	std::vector<std::vector<int>> GetFacesCopy();
	glm::vec3* GetFaceNormal(int index);

	std::vector<std::vector<GLfloat>> GetTriFans();
	std::vector<std::vector<GLfloat>*> GetTriStrips();
	std::vector<GLfloat> GetTriangles();

	int MergeVertices();
	int MergeVertices(GLfloat threshold);

	void GenPosMat();
	void GenVertexBuffer(GLuint triangle_mode);

	void RegisterUniforms();
	void SetUniforms();
};