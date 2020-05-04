#include <wx/wxprec.h>
#include "Model.h"

Model::Model() : Positionable(), Rotatable(), Nameable(), Scalable()
{
	this->m_shader_program = nullptr; //make blank shader program so that proper errors are thrown
}

Model::Model(Model& copy_from) : Positionable(copy_from), Rotatable(copy_from), Nameable(copy_from), Scalable(copy_from)
{
	//copy geometry
	std::vector<std::vector<GLfloat>> vertices = copy_from.GetVerticesCopy();
	std::vector<GLfloat>* vertex_subvector;
	for (int i = 0; i < (int)vertices.size(); i++)
	{
		vertex_subvector = new std::vector<GLfloat>;
		vertex_subvector->reserve(vertices.at(i).size());
		for (size_t j = 0; j < vertices.at(i).size(); j++)
		{
			vertex_subvector->push_back(vertices.at(i).at(j));
		}
		this->m_vertices.push_back(vertex_subvector);
	}

	std::vector<std::vector<int>> faces = copy_from.GetFacesCopy();
	std::vector<int>* faces_subvector;
	std::vector<glm::vec2>* face_uv_source;
	std::vector<glm::vec2>* face_uv;
	for (int i = 0; i < (int)faces.size(); i++)
	{
		//vertices
		faces_subvector = new std::vector<int>;
		faces_subvector->reserve(faces.at(i).size());
		for (size_t j = 0; j < faces.at(i).size(); j++)
		{
			faces_subvector->push_back(faces.at(i).at(j));
		}
		this->m_faces.push_back(faces_subvector);

		//normal
		this->m_face_normals.push_back(new glm::vec3(*copy_from.GetFaceNormal(i)));

		//uv
		face_uv = new std::vector<glm::vec2>;
		face_uv_source = copy_from.GetFaceUV(i);
		for (size_t j = 0; j < face_uv_source->size(); j++)
		{
			face_uv->push_back(face_uv_source->at(j));
		}
		this->m_face_uvs.push_back(face_uv);
	}

	this->m_shader_program = nullptr;
}

Model::~Model()
{
	for (size_t i = 0; i < this->m_vertices.size(); i++)
	{
		delete this->m_vertices.at(i);
	}

	for (size_t i = 0; i < this->m_faces.size(); i++)
	{
		delete this->m_faces.at(i);
		delete this->m_face_normals.at(i);
		delete this->m_face_uvs.at(i);
	}

	if (this->m_shadow_shader_program != nullptr)
	{
		delete this->m_shadow_shader_program;
	}

	glDeleteBuffers(1, &this->m_vao);

	for (size_t i = 0; i < this->m_vertex_buffers.size(); i++)
	{
		glDeleteVertexArrays(1, &this->m_vertex_buffers.at(i));
	}
}

int Model::AddVertex(GLfloat x, GLfloat y, GLfloat z)
{
	std::vector<GLfloat>* vertex = new std::vector<GLfloat>;

	vertex->push_back(x);
	vertex->push_back(y);
	vertex->push_back(z);

	this->m_vertices.push_back(vertex);

	return (int)this->m_vertices.size() - 1;
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

	for (int i = 0; i < (int)this->m_vertices.size(); i++)
	{
		current_item = this->m_vertices.at(i);
		
		if ((current_item->at(0) == x) && (current_item->at(1) == y) && (current_item->at(2) == z))
		{
			return i;
		}
	}
	return -1;
}

int Model::FindVertex(std::vector<GLfloat> vertex)
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

int Model::AddFace(std::vector<int> vertex_indices, glm::vec3 normal, std::vector<glm::vec2> uv)
{
	//copy edge indices
	std::vector<int>* vertexes_copy = new std::vector<int>;

	for (size_t i = 0; i < vertex_indices.size(); i++)
	{
		vertexes_copy->push_back(vertex_indices.at(i));
	}

	this->m_faces.push_back(vertexes_copy);

	//copy normal
	this->m_face_normals.push_back(new glm::vec3(normal));

	//copy uv
	std::vector<glm::vec2>* uv_copy = new std::vector<glm::vec2>;
	for (size_t i = 0; i < uv.size(); i++)
	{
		uv_copy->push_back(uv.at(i));
	}
	this->m_face_uvs.push_back(uv_copy);

	//return face index
	return (int)this->m_faces.size() - 1;
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
		this->m_face_normals.erase(this->m_face_normals.begin() + index);
		this->m_face_uvs.erase(this->m_face_uvs.begin() + index);
		return true;
	}
	else
	{
		return false;
	}
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

glm::vec3* Model::GetFaceNormal(int index)
{
	return this->m_face_normals.at(index);
}

std::vector<glm::vec2>* Model::GetFaceUV(int index)
{
	return this->m_face_uvs.at(index);
}

std::vector<std::vector<GLfloat>> Model::GetTriFans()
{
	std::vector<std::vector<GLfloat>> trifans;
	std::vector<GLfloat> current_fan;
	std::vector<int>* current_face;

	for (size_t i = 0; i < this->m_faces.size(); i++)
	{
		current_face = this->m_faces.at(i);
		if (current_face->size() > 2)
		{
			current_fan.clear();
			for (size_t j = 0; j < this->m_faces.at(i)->size(); j++)
			{
				for (size_t k = 0; k < this->GetVertex(this->m_faces.at(i)->at(j))->size(); k++)
				{
					current_fan.push_back(this->GetVertex(this->m_faces.at(i)->at(j))->at(k));
				}
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

std::vector<GLfloat> Model::GetTriangles()
{
	std::vector<GLfloat> triangles;
	std::vector<GLfloat>* vertex;
	std::vector<int>* face_vertex_indices;

	std::vector<std::vector<int>> face_tri_verts;
	std::vector<glm::vec3> tri_vecs;

	std::vector<std::vector<glm::vec2>> face_tri_uvs;

	glm::vec3 ccw_normal;
	glm::vec3 face_normal;
	float normal_angle_diff;

	std::vector<glm::vec2>* face_uv;

	glm::vec3 tangent;
	glm::vec3 bitangent;
	glm::vec3 edge1;
	glm::vec3 edge2;
	glm::vec2 edgeuv1;
	glm::vec2 edgeuv2;

	for (int i = 0; i < (int)this->m_faces.size(); i++)
	{
		face_vertex_indices = this->m_faces.at(i);
		if ((int)face_vertex_indices->size() > 2) //lines aren't faces and shouldn't be rendered
		{
			face_tri_verts.clear();
			face_tri_uvs.clear();

			face_uv = this->GetFaceUV(i);

			//get tris
			for (size_t j = 1; j < face_vertex_indices->size() - 1; j++)
			{
				face_tri_verts.push_back({ face_vertex_indices->at(0), face_vertex_indices->at(j), face_vertex_indices->at(j + 1) });
				face_tri_uvs.push_back({ face_uv->at(0), face_uv->at(j), face_uv->at(j + 1) });
			}

			//get face normal
			face_normal = *this->m_face_normals.at(i);

			//unpack tri data
			for (size_t j = 0; j < face_tri_verts.size(); j++)
			{
				//get as vectors
				tri_vecs.clear();
				for (size_t k = 0; k < face_tri_verts.at(j).size(); k++)
				{
					vertex = this->GetVertex(face_tri_verts.at(j).at(k));
					tri_vecs.push_back(glm::vec3(vertex->at(0), vertex->at(1), vertex->at(2)));
				}

				//calculate tangent and bitangent
				edge1 = tri_vecs.at(1) - tri_vecs.at(0);
				edge2 = tri_vecs.at(2) - tri_vecs.at(0);
				edgeuv1 = face_tri_uvs.at(j).at(1) - face_tri_uvs.at(j).at(0);
				edgeuv2 = face_tri_uvs.at(j).at(2) - face_tri_uvs.at(j).at(0);

				tangent.x = (edgeuv2.y * edge1.x) - (edgeuv1.y * edge2.x);
				tangent.y = (edgeuv2.y * edge1.y) - (edgeuv1.y * edge2.y);
				tangent.z = (edgeuv2.y * edge1.z) - (edgeuv1.y * edge2.z);

				bitangent.x = (edgeuv1.x * edge2.x) - (edgeuv2.x * edge1.x);
				bitangent.y = (edgeuv1.x * edge2.y) - (edgeuv2.x * edge1.y);
				bitangent.z = (edgeuv1.x * edge2.z) - (edgeuv2.x * edge1.z);

				//calculate ccw normal
				ccw_normal = glm::cross(tri_vecs.at(1) - tri_vecs.at(0), tri_vecs.at(2) - tri_vecs.at(0)); //follows right hand rule
				normal_angle_diff = std::abs(std::fmod(
						std::acos(glm::dot(ccw_normal, face_normal) / (glm::length(ccw_normal) * glm::length(face_normal))) + glm::pi<float>(),
					glm::pi<float>() * 2.0f) - glm::pi<float>());

				if (normal_angle_diff > (glm::pi<float>() / 2.0f))
				{
					std::reverse(face_tri_verts.at(j).begin(), face_tri_verts.at(j).end());
					std::reverse(face_tri_uvs.at(j).begin(), face_tri_uvs.at(j).end());
				}

				//prep for array
				for (size_t k = 0; k < face_tri_verts.at(j).size(); k++)
				{
					vertex = this->GetVertex(face_tri_verts.at(j).at(k));
					for (size_t l = 0; l < vertex->size(); l++)
					{
						triangles.push_back(vertex->at(l));
					}

					triangles.push_back(face_normal.x);
					triangles.push_back(face_normal.y);
					triangles.push_back(face_normal.z);

					triangles.push_back(face_tri_uvs.at(j).at(k).x);
					triangles.push_back(face_tri_uvs.at(j).at(k).y);

					triangles.push_back(tangent.x);
					triangles.push_back(tangent.y);
					triangles.push_back(tangent.z);

					triangles.push_back(bitangent.x);
					triangles.push_back(bitangent.y);
					triangles.push_back(bitangent.z);
				}
			}
		}
	}

	return triangles;
}

int Model::MergeVertices()
{
	return this->MergeVertices(0.0f);
}

int Model::MergeVertices(GLfloat threshold)
{
	std::vector<int> merged_vertices;

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

	//change faces that use duplicated vertices to use the first vertex instance
	for (std::map<int, std::vector<int>>::iterator it = vertex_joins.begin(); it != vertex_joins.end(); it++)
	{
		for (int i = 0; i < (int)it->second.size(); i++)
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
	//remove unreferenced vertices
	for (int i = (int)merged_vertices.size() - 1; i >= 0; i--)
	{
		delete this->m_vertices.at(i);
		this->m_vertices.erase(this->m_vertices.begin() + i);
	}*/

	return (int)merged_vertices.size();
}

void Model::GenVertexBuffer(GLuint triangle_mode)
{
	//delete left over arrays and buffers
	if (this->m_vao != NULL)
	{
		glDeleteVertexArrays(1, &this->m_vao);
	}

	glGenVertexArrays(1, &this->m_vao);
	glBindVertexArray(this->m_vao);

	for (int i = 0; i < (int)this->m_vertex_buffers.size(); i++)
	{
		glDeleteBuffers(1, &this->m_vertex_buffers.at(i));
	}
	this->m_vertex_buffers.clear();
	this->m_vertex_buffers_count.clear();

	//get verts in correct format
	this->m_triangle_mode = triangle_mode;

	if (triangle_mode == GL_TRIANGLES) //the only supported mode
	{
		std::vector<GLfloat> triangles = this->GetTriangles();

		GLfloat* tris = triangles.data();
		int size = (int)triangles.size();

		GLuint vertex_buffer;
		glGenBuffers(1, &vertex_buffer);
		glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
		glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * triangles.size(), triangles.data(), GL_STATIC_DRAW);
		this->m_vertex_buffers.push_back(vertex_buffer);
		this->m_vertex_buffers_count.push_back((int)triangles.size() / 14);

		glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 14 * sizeof(float), 0);
		glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 14 * sizeof(float), (void*)(3 * sizeof(float)));
		glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 14 * sizeof(float), (void*)(6 * sizeof(float)));
		glVertexAttribPointer(3, 3, GL_FLOAT, GL_FALSE, 14 * sizeof(float), (void*)(8 * sizeof(float)));
		glVertexAttribPointer(4, 3, GL_FLOAT, GL_FALSE, 14 * sizeof(float), (void*)(11 * sizeof(float)));

		glEnableVertexAttribArray(0);
		glEnableVertexAttribArray(1);
		glEnableVertexAttribArray(2);
		glEnableVertexAttribArray(3);
		glEnableVertexAttribArray(4);
	}
}

void Model::RegisterUniforms()
{
	this->m_shader_program->RegisterUniform("mdl_rotate");
	this->m_shader_program->RegisterUniform("mdl_translate");
	this->m_shader_program->RegisterUniform("mdl_scale");

	this->m_material.RegisterUniforms(this->m_shader_program);
}

void Model::SetUniforms()
{
	if (this->CheckIfRepositioned(true))
	{
		this->m_position_translate_vector = glm::vec4(this->GetPosition(0), this->GetPosition(1), this->GetPosition(2), 0.0f);
	}

	if (this->CheckIfRotated(true))
	{
		this->m_position_rotate_matrix = glm::mat4(1.0f);
		this->m_position_rotate_matrix = glm::rotate(this->m_position_rotate_matrix, glm::radians(this->GetRotation(0)), glm::vec3(1.0f, 0.0f, 0.0f));
		this->m_position_rotate_matrix = glm::rotate(this->m_position_rotate_matrix, glm::radians(this->GetRotation(1)), glm::vec3(0.0f, 1.0f, 0.0f));
		this->m_position_rotate_matrix = glm::rotate(this->m_position_rotate_matrix, glm::radians(this->GetRotation(2)), glm::vec3(0.0f, 0.0f, 1.0f));
	}

	if (this->CheckIfRescaled(true))
	{
		this->m_position_scale_matrix = glm::mat4(1.0f);
		this->m_position_scale_matrix = glm::scale(this->m_position_scale_matrix, glm::vec3(this->GetScale(0), this->GetScale(1), this->GetScale(2)));
	}

	glUniformMatrix4fv(this->m_shader_program->GetUniform("mdl_rotate"), 1, GL_FALSE, glm::value_ptr(this->m_position_rotate_matrix));
	glUniformMatrix4fv(this->m_shader_program->GetUniform("mdl_scale"), 1, GL_FALSE, glm::value_ptr(this->m_position_scale_matrix));
	glUniform4fv(this->m_shader_program->GetUniform("mdl_translate"), 1, glm::value_ptr(this->m_position_translate_vector));

	this->m_material.SetUniforms(this->m_shader_program);
}

void Model::SetShaderProgram(ShaderProgram* shader_program)
{
	if (this->m_shader_program != nullptr)
	{
		delete this->m_shader_program;
	}

	this->m_shader_program = shader_program;
}

ShaderProgram* Model::GetShaderProgram()
{
	return this->m_shader_program;
}

void Model::BindVAO()
{
	glBindVertexArray(this->m_vao);
}

void Model::DrawVBOs()
{
	for (size_t j = 0; j < this->m_vertex_buffers_count.size(); j++)
	{
		glBindBuffer(GL_ARRAY_BUFFER, this->m_vertex_buffers.at(j));
		glDrawArrays(this->m_triangle_mode, 0, this->m_vertex_buffers_count.at(j));
	}
}

void Model::SetMaterial(Material material)
{
	this->m_material = material;
}

void Model::InvertNormals()
{
	glm::vec3* normal;
	for (size_t i = 0; i < this->m_face_normals.size(); i++)
	{
		normal = this->m_face_normals.at(i);
		normal->x = 0.0f - normal->x;
		normal->y = 0.0f - normal->y;
		normal->z = 0.0f - normal->z;
	}
}

void Model::SetShadowShaderProgram(ShaderProgram* shader_program)
{
	if (this->m_shadow_shader_program != nullptr)
	{
		delete this->m_shadow_shader_program;
	}

	this->m_shadow_shader_program = shader_program;
}

ShaderProgram* Model::GetShadowShaderProgram()
{
	return this->m_shadow_shader_program;
}

void Model::RegisterShadowUniforms()
{
	this->m_shadow_shader_program->RegisterUniform("mdl_rotate");
	this->m_shadow_shader_program->RegisterUniform("mdl_translate");
	this->m_shadow_shader_program->RegisterUniform("mdl_scale");
}

void Model::SetShadowUniforms()
{
	if (this->CheckIfRepositioned(true))
	{
		this->m_position_translate_vector = glm::vec4(this->GetPosition(0), this->GetPosition(1), this->GetPosition(2), 0.0f);
	}

	if (this->CheckIfRotated(true))
	{
		this->m_position_rotate_matrix = glm::mat4(1.0f);
		this->m_position_rotate_matrix = glm::rotate(this->m_position_rotate_matrix, glm::radians(this->GetRotation(0)), glm::vec3(1.0f, 0.0f, 0.0f));
		this->m_position_rotate_matrix = glm::rotate(this->m_position_rotate_matrix, glm::radians(this->GetRotation(1)), glm::vec3(0.0f, 1.0f, 0.0f));
		this->m_position_rotate_matrix = glm::rotate(this->m_position_rotate_matrix, glm::radians(this->GetRotation(2)), glm::vec3(0.0f, 0.0f, 1.0f));
	}

	if (this->CheckIfRescaled(true))
	{
		this->m_position_scale_matrix = glm::mat4(1.0f);
		this->m_position_scale_matrix = glm::scale(this->m_position_scale_matrix, glm::vec3(this->GetScale(0), this->GetScale(1), this->GetScale(2)));
	}

	glUniformMatrix4fv(this->m_shadow_shader_program->GetUniform("mdl_rotate"), 1, GL_FALSE, glm::value_ptr(this->m_position_rotate_matrix));
	glUniformMatrix4fv(this->m_shadow_shader_program->GetUniform("mdl_scale"), 1, GL_FALSE, glm::value_ptr(this->m_position_scale_matrix));
	glUniform4fv(this->m_shadow_shader_program->GetUniform("mdl_translate"), 1, glm::value_ptr(this->m_position_translate_vector));
}