#pragma once

#include <vector>
#include <functional>
#include <tuple>

#include <glm/glm.hpp>

#include "Geometry.h"

class Polygonal : public Geometry
{
public:
	class Face
	{
	public:
		struct IndexedVertex
		{
			int vertex = -1;
			glm::dvec2 uv = glm::dvec2(0.0);

			bool operator==(const IndexedVertex& second);
			bool operator!=(const IndexedVertex& second);
		};

		struct StandaloneVertex
		{
			glm::dvec3 vertex = glm::dvec3(0.0);
			glm::dvec2 uv = glm::dvec2(0.0);

			bool operator==(const StandaloneVertex& second);
			bool operator!=(const StandaloneVertex& second);
		};

	private:
		Polygonal& m_parent;

		std::vector<IndexedVertex> m_vertices;

		glm::dvec3 m_normal = glm::dvec3(1.0, 0.0, 0.0);

	public:
		Face(Polygonal& parent);

		void AddVertex(IndexedVertex vertex);
		int AddVertex(StandaloneVertex vertex);
		void FilterVertices(std::function<bool(IndexedVertex vertex)> filter);
		void FilterVertices(std::function<bool(StandaloneVertex vertex)> filter);

		template<typename T>
		inline T FoldVertices(std::function<T(T previous, const IndexedVertex& vertex)> fold, T initial) const
		{
			for (const IndexedVertex& vertex : this->m_vertices)
			{
				initial = fold(initial, vertex);
			}
			return initial;
		}

		template<typename T>
		inline T FoldVertices(std::function<T(T previous, const StandaloneVertex& vertex)> fold, T initial) const
		{
			for (const IndexedVertex& vertex : this->m_vertices)
			{
				initial = fold(initial, this->GetStandaloneVertex(vertex));
			}
			return initial;
		}

		std::vector<IndexedVertex> GetIndexedVertices() const;
		std::vector<StandaloneVertex> GetStandaloneVertices() const;

		StandaloneVertex GetStandaloneVertex(const IndexedVertex& vertex) const;

		void SetNormal(glm::dvec3 normal);
		glm::dvec3 GetNormal() const;

		bool operator==(const Face& second) const;
		bool operator!=(const Face& second) const;
	};

private:
	std::vector<Face> m_faces;
	std::vector<glm::dvec3> m_vertices;

public:
	void AddFace(Face face);
	void FilterFaces(std::function<bool(const Face& face)> filter);
	void MapFaces(std::function<Face(const Face& face)> map);

	template<typename T>
	inline T FoldFaces(std::function<T(T previous, const Face& face)> fold, T initial) const
	{
		for (const Face& face : this->m_faces)
		{
			initial = fold(initial, face);
		}
		return initial;
	}

	int AddVertex(glm::dvec3 vertex);
	void PruneVertices();
	void FilterVertices(std::function<bool(glm::dvec3 vertex)> filter);
	void MapVertices(std::function<glm::dvec3(glm::dvec3 vertex)> map);
	
	template<typename T>
	inline T FoldVertices(std::function<T(T previous, glm::dvec3 vertex)> fold, T initial) const
	{
		for (glm::dvec3 vertex : this->m_vertices)
		{
			initial = fold(initial, vertex);
		}
		return initial;
	}

	void MergeVertices(double threshold = 0.0);
	void InvertNormals();

	std::vector<double> GetTriangles() const override;

	bool operator==(const Polygonal& second) const;
	bool operator!=(const Polygonal& second) const;
};
