#include "Model.h"

#include <array>
#include <cmath>
#include <set>
#include <map>

#include "../Scene.h"

Model::Model(ModelReference reference, ModelGeometry geometry, Scene* scene) : Positionable(), Rotatable(), Nameable(), Scalable(), Referenceable<ModelReference>(reference)
{
	this->m_geometry = geometry;
	this->m_scene = scene;

	if (scene == nullptr)
	{
		this->m_texture_colour = LocalTexture(-1);
		this->m_texture_reflection = LocalTexture(-1);
		this->m_texture_specular = LocalTexture(-1);
		this->m_texture_normal = LocalTexture(-1);
	}
	else
	{
		this->m_texture_colour = LocalTexture(scene->GetNewTextureReference());
		this->m_texture_reflection = LocalTexture(scene->GetNewTextureReference());
		this->m_texture_specular = LocalTexture(scene->GetNewTextureReference());
		this->m_texture_normal = LocalTexture(scene->GetNewTextureReference());
	}
}

std::vector<std::vector<double>> Model::GetTriFans() const
{
	throw std::logic_error("Not implemented");
}

std::vector<std::vector<double>> Model::GetTriStrips() const
{
	throw std::logic_error("Not implemented");
}

std::vector<double> Model::GetTriangles(bool only_geometry) const
{
	return ::GetTriangles(this->m_geometry, only_geometry);
}

Material& Model::GetMaterial()
{
	return this->m_material;
}

const ModelGeometry& Model::GetGeometry() const
{
	return this->m_geometry;
}

LocalTexture& Model::GetColourTexture()
{
	return this->m_texture_colour;
}

LocalTexture& Model::GetReflectionTexture()
{
	return this->m_texture_reflection;
}

LocalTexture& Model::GetSpecularTexture()
{
	return this->m_texture_specular;
}

LocalTexture& Model::GetNormalTexture()
{
	return this->m_texture_normal;
}

LocalTexture& Model::GetSkyboxMaskTexture()
{
	return this->m_texture_skybox_mask;
}

void Model::SetWireframeColour(glm::vec3 colour)
{
	this->m_wireframe_colour = colour;
}

void Model::SetWireframeColourSelected()
{
	this->SetWireframeColour(glm::vec3(1.0f, 0.75f, 0.0f));
}

void Model::SetWireframeColourUnselected()
{
	this->SetWireframeColour(glm::vec3(0.0f));
}

glm::vec3 Model::GetWireframeColour() const
{
	return this->m_wireframe_colour;
}

void Model::SetSkybox(Skybox* skybox)
{
	this->m_skybox = skybox;
}

Skybox* Model::GetSkybox()
{
	return this->m_skybox;
}

bool operator==(const ModelGeometry& first, const ModelGeometry& second)
{
	//faces
	if (first.faces.size() != second.faces.size())
	{
		return false;
	}

	for (int i = 0; i < (int)first.faces.size(); i++)
	{
		if (first.faces.at(i) != second.faces.at(i))
		{
			return false;
		}
	}

	//vertices
	if (first.vertices.size() != second.vertices.size())
	{
		return false;
	}

	for (int i = 0; i < (int)first.vertices.size(); i++)
	{
		if (first.vertices.at(i) != second.vertices.at(i))
		{
			return false;
		}
	}

	//all tests passed, default state (they are the same)
	return true;
}

bool operator!=(const ModelGeometry& first, const ModelGeometry& second)
{
	return !(first == second);
}

bool operator==(const Face& first, const Face& second)
{
	//normal
	if (first.normal != second.normal)
	{
		return false;
	}

	//vertices
	if (first.vertices.size() != second.vertices.size())
	{
		return false;
	}

	for (int i = 0; i < (int)first.vertices.size(); i++)
	{
		if (first.vertices.at(i) != second.vertices.at(i))
		{
			return false;
		}
	}

	//uvs
	if (first.uv.size() != second.uv.size())
	{
		return false;
	}

	for (int i = 0; i < (int)first.uv.size(); i++)
	{
		if (first.uv.at(i) != second.uv.at(i))
		{
			return false;
		}
	}

	//all tests passed, default state (they are the same)
	return true;
}

bool operator!=(const Face& first, const Face& second)
{
	return !(first == second);
}

void MergeVertices(ModelGeometry& geometry, double threshold)
{
	//make several groups of points that are within the threshold from each other (this is approximate)
	//the first vertex to be added to the group is the one that is preserved, all others map onto it
	std::vector<std::vector<int>> groups;
	for (int i = 0; i < static_cast<int>(geometry.vertices.size()); i++)
	{
		bool group_found = false;
		int group_index;

		for (int j = 0; j < static_cast<int>(groups.size()); j++)
		{
			if (glm::length(geometry.vertices.at(groups.at(j).at(0)) - geometry.vertices.at(i)) <= threshold) //a group has been found
			{
				group_found = true;
				group_index = j;
				j = static_cast<int>(groups.size()); //exit loop, no need to look for another group
			}
		}

		if (group_found)
		{
			groups.at(group_index).push_back(i);
		}
		else
		{
			groups.push_back(std::vector(1, i));
		}
	}

	//create a more performant way of looking up the primary vertex from a group member
	std::map<int, int> group_lookup;
	for (int i = 0; i < static_cast<int>(groups.size()); i++)
	{
		for (int member_index : groups.at(i))
		{
			group_lookup.insert(std::pair(member_index, i));
		}
	}

	//create final list of vertices that contains the primary vertex of each group
	std::vector<glm::dvec3> vecs_copy = geometry.vertices;
	geometry.vertices.clear();
	for (std::vector<int> group : groups)
	{
		geometry.vertices.push_back(vecs_copy.at(group.at(0)));
	}

	//write the index of the primary vertex to any faces that reference the secondary vertex
	for (Face& face : geometry.faces)
	{
		for (int& vertex_index : face.vertices)
		{
			vertex_index = group_lookup.at(vertex_index);
		}
	}
}

void InvertNormals(ModelGeometry& geometry)
{
	for (int i = 0; i < (int)geometry.faces.size(); i++)
	{
		geometry.faces.at(i).normal = 0.0 - geometry.faces.at(i).normal;
	}
}

std::vector<double> GetTriangles(const ModelGeometry& geometry, bool only_geometry)
{
	std::vector<double> triangles;

	for (const Face& face : geometry.faces)
	{
		if (face.vertices.size() > 2) //lines aren't faces, don't draw them
		{
			std::vector<glm::ivec3> tri_indices;
			for (int j = 0; j < (int)face.vertices.size() - 2; j++)
			{
				tri_indices.push_back(glm::ivec3(0, j + 1, j + 2));
			}

			for (int j = 0; j < (int)tri_indices.size(); j++)
			{
				glm::ivec3& indices = tri_indices.at(j);
				std::array<glm::dvec3, 3> tri_vecs = {
					geometry.vertices.at(face.vertices.at(indices[0])),
					geometry.vertices.at(face.vertices.at(indices[1])),
					geometry.vertices.at(face.vertices.at(indices[2]))
				};
				std::array<glm::dvec2, 3> tri_uvs = {
					face.uv.at(indices[0]),
					face.uv.at(indices[1]),
					face.uv.at(indices[2])
				};

				//calculate ccw normal
				glm::dvec3 ccw_normal = glm::cross(tri_vecs.at(1) - tri_vecs.at(0), tri_vecs.at(2) - tri_vecs.at(0)); //follows right hand rule
				double normal_angle_diff = std::abs(std::fmod(
					std::acos(glm::dot(ccw_normal, face.normal) / (glm::length(ccw_normal) * glm::length(face.normal))) + glm::pi<double>(),
					glm::pi<double>() * 2.0f) - glm::pi<double>());

				if (normal_angle_diff > (glm::pi<double>() * 0.5))
				{
					std::reverse(tri_vecs.begin(), tri_vecs.end());
					std::reverse(tri_uvs.begin(), tri_uvs.end());
				}

				//calculate tangent and bitangent
				glm::dvec3 edge1 = tri_vecs.at(1) - tri_vecs.at(0);
				glm::dvec3 edge2 = tri_vecs.at(2) - tri_vecs.at(0);
				glm::dvec2 edgeuv1 = tri_uvs.at(1) - tri_uvs.at(0);
				glm::dvec2 edgeuv2 = tri_uvs.at(2) - tri_uvs.at(0);

				glm::dvec3 tangent;
				tangent.x = (edgeuv2.y * edge1.x) - (edgeuv1.y * edge2.x);
				tangent.y = (edgeuv2.y * edge1.y) - (edgeuv1.y * edge2.y);
				tangent.z = (edgeuv2.y * edge1.z) - (edgeuv1.y * edge2.z);

				glm::dvec3 bitangent;
				bitangent.x = (edgeuv1.x * edge2.x) - (edgeuv2.x * edge1.x);
				bitangent.y = (edgeuv1.x * edge2.y) - (edgeuv2.x * edge1.y);
				bitangent.z = (edgeuv1.x * edge2.z) - (edgeuv2.x * edge1.z);

				//push data to gl-ready vector
				for (int k = 0; k < (int)tri_vecs.size(); k++)
				{
					triangles.push_back(tri_vecs.at(k).x);
					triangles.push_back(tri_vecs.at(k).y);
					triangles.push_back(tri_vecs.at(k).z);

					if (!only_geometry)
					{
						triangles.push_back(face.normal.x);
						triangles.push_back(face.normal.y);
						triangles.push_back(face.normal.z);

						triangles.push_back(tri_uvs.at(k).x);
						triangles.push_back(tri_uvs.at(k).y);

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
	}

	return triangles;
}

std::vector<GLfloat> DoubleToSinglePrecision(std::vector<double> vec)
{
	std::vector<GLfloat> result;
	result.reserve(vec.size());

	for (int i = 0; i < (int)vec.size(); i++)
	{
		result.push_back((GLfloat)vec.at(i));
	}

	return result;
}
