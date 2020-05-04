#pragma once

#include <wx/wxprec.h>
#include "../../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <string>
#include <vector>
#include <map>
#include <array>
#include <tuple>
#include <string>
#include <cmath>

#include "../Positionable.h"
#include "../Rotatable.h"
#include "../Scalable.h"
#include "../Nameable.h"
#include "../../render/ShaderProgram.h"
#include "Material.h"

class Model : public Positionable, public Rotatable, public Scalable, public Nameable
{
private:
	std::vector<std::vector<GLfloat>*> m_vertices;
	std::vector<std::vector<int>*> m_faces;
	std::vector<glm::vec3*> m_face_normals;
	std::vector<std::vector<glm::vec2>*> m_face_uvs;

	ShaderProgram* m_shader_program = nullptr;
	ShaderProgram* m_shadow_shader_program = nullptr;

	glm::mat4 m_position_rotate_matrix = glm::mat4(1.0f);
	glm::mat4 m_position_scale_matrix = glm::mat4(1.0f);
	glm::vec4 m_position_translate_vector = glm::vec4(0.0f);

	GLuint m_triangle_mode = GL_TRIANGLES;
	GLuint m_vao = NULL;
	std::vector<GLuint> m_vertex_buffers;
	std::vector<GLint> m_vertex_buffers_count; //number of sets of vertices (6 floats means 2 sets)

	Material m_material;

public:
	Model();
	Model(Model& copy_from);
	~Model();

	//vertices
	int AddVertex(GLfloat x, GLfloat y, GLfloat z);
	int AddVertex(std::vector<GLfloat> vertex);
	bool RemoveVertex(int index);
	bool RemoveVertex(size_t index);
	int FindVertex(GLfloat x, GLfloat y, GLfloat z);
	int FindVertex(std::vector<GLfloat> vertex);
	std::vector<GLfloat>* GetVertex(int index);
	std::vector<std::vector<GLfloat>> GetVerticesCopy();

	//faces, face normals and face uvs
	int AddFace(std::vector<int> vertex_indices, glm::vec3 normal, std::vector<glm::vec2> uv);
	bool RemoveFace(int index);
	bool RemoveFace(size_t index);
	std::vector<int>* GetFace(int index);
	std::vector<std::vector<int>> GetFacesCopy();
	glm::vec3* GetFaceNormal(int index);
	std::vector<glm::vec2>* GetFaceUV(int index);

	std::vector<std::vector<GLfloat>> GetTriFans();
	std::vector<std::vector<GLfloat>*> GetTriStrips();
	std::vector<GLfloat> GetTriangles();

	int MergeVertices();
	int MergeVertices(GLfloat threshold);

	void GenVertexBuffer(GLuint triangle_mode);

	void RegisterUniforms();
	void SetUniforms();

	void RegisterShadowUniforms();
	void SetShadowUniforms();

	void SetShaderProgram(ShaderProgram* shader_program);
	ShaderProgram* GetShaderProgram();
	void SetShadowShaderProgram(ShaderProgram* shader_program);
	ShaderProgram* GetShadowShaderProgram();
	void BindVAO();
	void DrawVBOs();

	void SetMaterial(Material material);

	void InvertNormals();
};