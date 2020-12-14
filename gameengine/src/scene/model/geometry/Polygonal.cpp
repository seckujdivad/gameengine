#include "Polygonal.h"

#include <stdexcept>
#include <array>

#include <glm/ext.hpp>

Polygonal::Face::IndexedVertex::IndexedVertex(int vertex, glm::dvec2 uv) : vertex(vertex), uv(uv)
{
}

bool Polygonal::Face::IndexedVertex::operator==(const IndexedVertex& second) const
{
	if (this->vertex != second.vertex)
	{
		return false;
	}

	if (this->uv != second.uv)
	{
		return false;
	}

	return true;
}

bool Polygonal::Face::IndexedVertex::operator!=(const IndexedVertex& second) const
{
	return !(*this == second);
}

Polygonal::Face::StandaloneVertex::StandaloneVertex(glm::dvec3 vertex, glm::dvec2 uv) : vertex(vertex), uv(uv)
{
}

bool Polygonal::Face::StandaloneVertex::operator==(const StandaloneVertex& second) const
{
	if (this->vertex != second.vertex)
	{
		return false;
	}

	if (this->uv != second.uv)
	{
		return false;
	}

	return true;
}

bool Polygonal::Face::StandaloneVertex::operator!=(const StandaloneVertex& second) const
{
	return !(*this == second);
}

Polygonal::Face::Face(Polygonal& parent) : m_parent(parent)
{
}

Polygonal::Face::Face(const Face& copy_from) : m_parent(copy_from.m_parent)
{
	*this = copy_from;
}

Polygonal::Face& Polygonal::Face::operator=(const Face& copy_from)
{
	this->m_normal = copy_from.m_normal;
	this->m_vertices = copy_from.m_vertices;

	return *this;
}

Polygonal::Face::Face(Face&& move_from) noexcept : m_parent(move_from.m_parent)
{
	*this = std::move(move_from);
}

Polygonal::Face& Polygonal::Face::operator=(Face&& move_from) noexcept
{
	if (this == &move_from)
	{
		return *this;
	}

	this->m_normal = move_from.m_normal;
	this->m_vertices = move_from.m_vertices;

	return *this;
}

void Polygonal::Face::AddVertex(IndexedVertex vertex)
{
	if (!this->m_parent.VertexDoesExist(vertex.vertex))
	{
		throw std::invalid_argument("Indexed vertex does not exist in parent");
	}

	this->m_vertices.push_back(vertex);
}

void Polygonal::Face::AddVertex(StandaloneVertex vertex)
{
	this->AddVertex(this->GetIndexedVertex(vertex));
}

void Polygonal::Face::FilterVertices(std::function<bool(IndexedVertex vertex)> filter)
{
	FilterVectorInPlace(this->m_vertices, filter);
}

void Polygonal::Face::MapVertices(std::function<IndexedVertex(IndexedVertex vertex)> map)
{
	MapVectorInPlace(this->m_vertices, map);
}

std::vector<Polygonal::Face::IndexedVertex> Polygonal::Face::GetIndexedVertices() const
{
	return this->m_vertices;
}

std::vector<Polygonal::Face::StandaloneVertex> Polygonal::Face::GetStandaloneVertices() const
{
	std::vector<StandaloneVertex> result;
	result.reserve(this->m_vertices.size());
	for (const IndexedVertex& vertex : this->m_vertices)
	{
		result.push_back(this->GetStandaloneVertex(vertex));
	}
	return result;
}

Polygonal::Face::StandaloneVertex Polygonal::Face::GetStandaloneVertex(const IndexedVertex& vertex) const
{
	return StandaloneVertex(this->m_parent.GetVertex(vertex.vertex), vertex.uv);
}

Polygonal::Face::IndexedVertex Polygonal::Face::GetIndexedVertex(const StandaloneVertex& vertex) const
{
	IndexedVertex indexed_vertex;
	indexed_vertex.uv = vertex.uv;

	indexed_vertex.vertex = this->m_parent.GetVertexIndex(vertex.vertex, false);
	
	if (indexed_vertex.vertex == -1)
	{
		indexed_vertex.vertex = this->m_parent.AddVertex(vertex.vertex);
	}

	return indexed_vertex;
}

int Polygonal::Face::GetNumVertices() const
{
	return static_cast<int>(this->m_vertices.size());
}

void Polygonal::Face::SetNormal(glm::dvec3 normal)
{
	this->m_normal = normal;
}

glm::dvec3 Polygonal::Face::GetNormal() const
{
	return this->m_normal;
}

bool Polygonal::Face::operator==(const Face& second) const
{
	if (this->m_parent != second.m_parent)
	{
		return false;
	}

	if (this->m_normal != second.m_normal)
	{
		return false;
	}

	if (this->m_vertices != second.m_vertices)
	{
		return false;
	}

	return true;
}

bool Polygonal::Face::operator!=(const Face& second) const
{
	return !(*this == second);
}

void Polygonal::AddFace(Face face)
{
	this->m_faces.push_back(face);
}

std::vector<Polygonal::Face> Polygonal::GetFaces() const
{
	return this->m_faces;
}

void Polygonal::FilterFaces(std::function<bool(const Face& face)> filter)
{
	FilterVectorInPlace(this->m_faces, filter);
}

void Polygonal::MapFaces(std::function<Face(const Face& face)> map)
{
	MapVectorInPlace(this->m_faces, map);
}

int Polygonal::AddVertex(glm::dvec3 vertex)
{
	int value = this->GetVertexIndex(vertex, false);

	if (value == -1)
	{
		value = this->m_vertices_counter;
		this->m_vertices.insert(std::pair(this->m_vertices_counter++, vertex));
	}

	return value;
}

glm::dvec3 Polygonal::GetVertex(int identifier) const
{
	return this->m_vertices.at(identifier);
}

int Polygonal::GetVertexIndex(glm::dvec3 vertex, bool do_throw) const
{
	for (const auto& [key, value] : this->m_vertices)
	{
		if (value == vertex)
		{
			return key;
		}
	}

	if (do_throw)
	{
		throw std::invalid_argument("Vertex not found");
	}
	else
	{
		return -1;
	}
}

std::vector<glm::dvec3> Polygonal::GetVertices() const
{
	std::vector<glm::dvec3> result;
	for (const auto& [key, value] : this->m_vertices)
	{
		result.push_back(value);
	}
	return result;
}

void Polygonal::RemoveVertex(int identifier)
{
	for (const Face& face : this->m_faces)
	{
		for (const Face::IndexedVertex& vertex : face.GetIndexedVertices())
		{
			if (vertex.vertex == identifier)
			{
				throw std::invalid_argument("There are still faces that depend on this vertex");
			}
		}
	}

	this->m_vertices.erase(identifier);
}

void Polygonal::RemoveVertex(glm::dvec3 vertex)
{
	this->RemoveVertex(this->GetVertexIndex(vertex));
}

bool Polygonal::VertexDoesExist(int identifier)
{
	return this->m_vertices.count(identifier) != 0;
}

void Polygonal::PruneVertices()
{
	std::unordered_map<int, int> identifier_lookup;
	std::unordered_map<int, glm::dvec3> new_vertices;
	int vertices_counter = 0;
	for (const auto& [key, value] : this->m_vertices)
	{
		new_vertices.insert(std::pair(static_cast<int>(vertices_counter++), value));
		identifier_lookup.insert(std::pair(key, static_cast<int>(vertices_counter++)));
	}

	this->m_vertices = new_vertices;
	this->m_vertices_counter = vertices_counter;

	for (Face& face : this->m_faces)
	{
		face.MapVertices([identifier_lookup](Face::IndexedVertex vertex)
			{
				vertex.vertex = identifier_lookup.at(vertex.vertex);
				return vertex;
			});
	}
}

void Polygonal::FilterVertices(std::function<bool(int identifier, glm::dvec3 vertex)> filter)
{
	for (const auto& [identifier, vertex] : this->m_vertices)
	{
		if (!filter(identifier, vertex))
		{
			this->m_vertices.erase(identifier);
		}
	}
}

void Polygonal::MapVertices(std::function<std::pair<int, glm::dvec3>(int identifier, glm::dvec3 vertex)> map)
{
	for (auto& [identifier, vertex] : this->m_vertices)
	{
		std::pair<int, glm::dvec3> result = map(identifier, vertex);

		if (result.first == identifier)
		{
			vertex = result.second;
		}
		else
		{
			this->RemoveVertex(identifier); //automatically makes sure that this vertex has no face dependents
			this->m_vertices.insert(result);
		}
	}
}

void Polygonal::MergeVertices(double threshold)
{
	std::unordered_map<int, int> new_vertex_lookup;
	for (const auto& [key, value] : this->m_vertices)
	{
		bool map_found = false;
		for (const auto& [mapped_index, preserved_index] : new_vertex_lookup)
		{
			if (!map_found && glm::length(this->GetVertex(preserved_index) - value) <= threshold)
			{
				new_vertex_lookup.insert(std::pair(key, preserved_index));
				map_found = true;
			}
		}

		if (!map_found)
		{
			new_vertex_lookup.insert(std::pair(key, key));
		}
	}

	for (Face& face : this->m_faces)
	{
		face.MapVertices(
			[new_vertex_lookup](Face::IndexedVertex vertex)
			{
				vertex.vertex = new_vertex_lookup.at(vertex.vertex);
				return vertex;
			}
		);
	}

	this->PruneVertices();
}

void Polygonal::InvertNormals()
{
	for (auto& face : this->m_faces)
	{
		face.SetNormal(0.0 - face.GetNormal());
	}
}

std::vector<double> Polygonal::GetTriangles() const
{
	std::vector<double> result;
	{
		size_t result_size = 0;
		for (const Face& face : this->m_faces)
		{
			result_size += static_cast<size_t>(face.GetNumVertices() * GAMEENGINE_VALUES_PER_VERTEX);
		}
		result.reserve(result_size);
	}

	for (const Face& face : this->m_faces)
	{
		const std::vector<Face::StandaloneVertex> vertices = face.GetStandaloneVertices();
		if (vertices.size() > 2) //lines aren't faces, don't draw them
		{
			//split the polygon into a number of triangles - generate the vertex indices of these triangles
			std::vector<glm::ivec3> tri_indices;
			for (int i = 0; i < static_cast<int>(vertices.size()) - 2; i++)
			{
				tri_indices.push_back(glm::ivec3(0, i + 1, i + 2));
			}

			//add each generated triangle's data to the resulting vector of doubles
			for (const glm::ivec3& indices : tri_indices)
			{
				//resolve the triangle generated from the polygon
				std::array<Face::StandaloneVertex, 3> triangle = {
					vertices.at(indices[0]),
					vertices.at(indices[1]),
					vertices.at(indices[2])
				};

				//calculate ccw normal
				const glm::dvec3 ccw_normal = glm::cross(triangle.at(1).vertex - triangle.at(0).vertex, triangle.at(2).vertex - triangle.at(0).vertex); //follows right hand rule
				const double normal_angle_diff = std::abs(std::fmod(
					std::acos(glm::dot(ccw_normal, face.GetNormal()) / (glm::length(ccw_normal) * glm::length(face.GetNormal()))) + glm::pi<double>(),
					glm::pi<double>() * 2.0f) - glm::pi<double>());

				//reverse the vertex winding if the normal derived by assuming counter-clockwise winding is too far from the surface normal - this is how opengl will derive it
				if (normal_angle_diff > (glm::pi<double>() * 0.5))
				{
					std::reverse(triangle.begin(), triangle.end());
				}

				//calculate tangent and bitangent
				const glm::dvec3 edge1 = triangle.at(1).vertex - triangle.at(0).vertex;
				const glm::dvec3 edge2 = triangle.at(2).vertex - triangle.at(0).vertex;
				const glm::dvec2 edgeuv1 = triangle.at(1).uv - triangle.at(0).uv;
				const glm::dvec2 edgeuv2 = triangle.at(2).uv - triangle.at(0).uv;

				const glm::dvec3 tangent = glm::dvec3(
					(edgeuv2.y * edge1.x) - (edgeuv1.y * edge2.x),
					(edgeuv2.y * edge1.y) - (edgeuv1.y * edge2.y),
					(edgeuv2.y * edge1.z) - (edgeuv1.y * edge2.z)
				);

				const glm::dvec3 bitangent = glm::dvec3(
					(edgeuv1.x * edge2.x) - (edgeuv2.x * edge1.x),
					(edgeuv1.x * edge2.y) - (edgeuv2.x * edge1.y),
					(edgeuv1.x * edge2.z) - (edgeuv2.x * edge1.z)
				);

				//push data to gl-ready vector
				for (const Face::StandaloneVertex& vertex : triangle)
				{
					result.push_back(vertex.vertex.x);
					result.push_back(vertex.vertex.y);
					result.push_back(vertex.vertex.z);

					result.push_back(face.GetNormal().x);
					result.push_back(face.GetNormal().y);
					result.push_back(face.GetNormal().z);

					result.push_back(vertex.uv.x);
					result.push_back(vertex.uv.y);

					result.push_back(tangent.x);
					result.push_back(tangent.y);
					result.push_back(tangent.z);

					result.push_back(bitangent.x);
					result.push_back(bitangent.y);
					result.push_back(bitangent.z);
				}
			}
		}
	}

	return result;
}

bool Polygonal::operator==(const Polygonal& second) const
{
	if (this->m_faces != second.m_faces)
	{
		return false;
	}

	if (this->m_vertices != second.m_vertices)
	{
		return false;
	}

	return true;
}

bool Polygonal::operator!=(const Polygonal& second) const
{
	return !(*this == second);
}
