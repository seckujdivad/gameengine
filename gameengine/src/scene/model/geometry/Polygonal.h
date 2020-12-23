#pragma once

#include <vector>
#include <functional>
#include <unordered_map>

#include <glm/glm.hpp>

#include "Geometry.h"
#include "../../../generic/FunctionalVector.h"

class Polygonal : public Geometry
{
public:
	class Face
	{
	public:
		struct IndexedVertex
		{
			IndexedVertex(int vertex = -1, glm::dvec2 uv = glm::dvec2(0.0));

			int vertex = -1;
			glm::dvec2 uv = glm::dvec2(0.0);

			bool operator==(const IndexedVertex& second) const;
			bool operator!=(const IndexedVertex& second) const;
		};

		struct StandaloneVertex
		{
			StandaloneVertex(glm::dvec3 vertex = glm::dvec3(0.0), glm::dvec2 uv = glm::dvec2(0.0));

			glm::dvec3 vertex = glm::dvec3(0.0);
			glm::dvec2 uv = glm::dvec2(0.0);

			bool operator==(const StandaloneVertex& second) const;
			bool operator!=(const StandaloneVertex& second) const;
		};

	private:
		std::vector<IndexedVertex> m_vertices;
		glm::dvec3 m_normal = glm::dvec3(1.0, 0.0, 0.0);

		Polygonal& m_parent;

	public:
		Face(Polygonal& parent);
		Face(const Face& copy_from);
		Face& operator=(const Face& copy_from);
		Face(Face&& move_from) noexcept;
		Face& operator=(Face&& move_from) noexcept;

		void AddVertex(IndexedVertex vertex);
		void AddVertex(StandaloneVertex vertex);
		void FilterVertices(std::function<bool(IndexedVertex vertex)> filter);
		void FilterVertices(std::function<bool(StandaloneVertex vertex)> filter);
		void MapVertices(std::function<IndexedVertex(IndexedVertex vertex)> map);
		void MapVertices(std::function<StandaloneVertex(StandaloneVertex vertex)> map);

		template<typename T>
		inline T FoldVertices(std::function<T(T previous, const IndexedVertex& vertex)> fold, T initial) const
		{
			return FoldVector<IndexedVertex>(fold, this->m_vertices, initial);
		}

		template<typename T>
		inline T FoldVertices(std::function<T(T previous, const StandaloneVertex& vertex)> fold, T initial) const
		{
			return FoldVector<StandaloneVertex>(fold, this->GetStandaloneVertices(), initial);
		}

		std::vector<IndexedVertex> GetIndexedVertices() const;
		std::vector<StandaloneVertex> GetStandaloneVertices() const;

		StandaloneVertex GetStandaloneVertex(const IndexedVertex& vertex) const;
		IndexedVertex GetIndexedVertex(const StandaloneVertex& vertex) const;

		int GetNumVertices() const;

		void SetNormal(glm::dvec3 normal);
		glm::dvec3 GetNormal() const;

		bool operator==(const Face& second) const;
		bool operator!=(const Face& second) const;
	};

private:
	std::vector<Face> m_faces;
	std::unordered_map<int, glm::dvec3> m_vertices;
	int m_vertices_counter = 0;

	PrimitiveType m_output_primitive = PrimitiveType::Quads;

protected:
	std::vector<double> GetPrimitivesWithoutCache() const override;

public:
	void AddFace(Face face);
	std::vector<Face> GetFaces() const;
	void FilterFaces(std::function<bool(const Face& face)> filter);
	void MapFaces(std::function<Face(const Face& face)> map);

	template<typename T>
	inline T FoldFaces(std::function<T(T previous, const Face& face)> fold, T initial) const
	{
		return FoldVector(fold, this->m_faces, initial);
	}

	int AddVertex(glm::dvec3 vertex);
	glm::dvec3 GetVertex(int identifier) const;
	int GetVertexIndex(glm::dvec3 vertex, bool do_throw = true) const;
	std::vector<glm::dvec3> GetVertices() const;
	void RemoveVertex(int identifier);
	void RemoveVertex(glm::dvec3 vertex);
	bool VertexDoesExist(int identifier);
	void PruneVertices();
	void FilterVertices(std::function<bool(int identifier, glm::dvec3 vertex)> filter);
	void MapVertices(std::function<std::pair<int, glm::dvec3>(int identifier, glm::dvec3 vertex)> map);
	
	template<typename T>
	inline T FoldVertices(std::function<T(T previous, int identifier, glm::dvec3 vertex)> fold, T initial) const
	{
		for (const auto& [identifier, vertex] : this->m_vertices)
		{
			initial = fold(initial, identifier, vertex);
		}
		return initial;
	}

	void MergeVertices(double threshold = 0.0);
	void InvertNormals();
	
	std::size_t GetPrimitivesNumVertices() const override;
	Geometry::PrimitiveType GetPrimitiveType() const override;

	void SetPrimitiveType(Geometry::PrimitiveType type);

	bool operator==(const Polygonal& second) const;
	bool operator!=(const Polygonal& second) const;
};
