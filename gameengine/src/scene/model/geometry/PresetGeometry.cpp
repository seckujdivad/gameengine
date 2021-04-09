#include "PresetGeometry.h"

#include <stdexcept>
#include <string>

#include "Polygonal.h"
#include "../../../loaders/models/PlyLoader.h"
#include "../../../Resource.h"

std::vector<double> PresetGeometry::GetPrimitivesWithoutCache() const
{
	return this->m_geometry->GetPrimitives();
}

PresetGeometry::PresetGeometry(GeometryType type) : m_type(type)
{
	this->SetGeometryType(type, true);
}

void PresetGeometry::SetGeometryType(GeometryType type, bool force_reload)
{
	if (force_reload || (this->m_type != type))
	{
		this->m_type = type;
		this->InvalidatePrimitivesCache();

		if (type == GeometryType::Plane)
		{
			std::shared_ptr<Polygonal> geom = std::make_shared<Polygonal>();
			geom->SetPrimitiveType(Geometry::PrimitiveType::Triangles);

			Polygonal::Face face = Polygonal::Face(*geom);

			const double vertex_size = 1.0;
			const double uv_size = 1.0;
			face.AddVertex(Polygonal::Face::StandaloneVertex(glm::dvec3(-vertex_size, -vertex_size, 0.0), glm::dvec2(0.0, 0.0)));
			face.AddVertex(Polygonal::Face::StandaloneVertex(glm::dvec3(-vertex_size, vertex_size, 0.0), glm::dvec2(0.0, uv_size)));
			face.AddVertex(Polygonal::Face::StandaloneVertex(glm::dvec3(vertex_size, vertex_size, 0.0), glm::dvec2(uv_size, uv_size)));
			face.AddVertex(Polygonal::Face::StandaloneVertex(glm::dvec3(vertex_size, -vertex_size, 0.0), glm::dvec2(uv_size, 0.0)));

			face.SetNormal(glm::dvec3(0.0, 0.0, -1.0));

			geom->AddFace(face);

			this->m_geometry = geom;
		}
		else if (type == GeometryType::Icosphere)
		{
			this->m_geometry = ModelFromPlyText(GetEmbeddedTextfile(RCID_TF_MODEL_ICOSPHERE));
		}
		else
		{
			throw std::invalid_argument("Geometry type " + std::to_string(static_cast<int>(type)) + " is not recognised");
		}
	}
}

PresetGeometry::GeometryType PresetGeometry::GetGeometryType() const
{
	return this->m_type;
}

std::size_t PresetGeometry::GetPrimitivesNumVertices() const
{
	return this->m_geometry->GetPrimitivesNumVertices();
}

Geometry::PrimitiveType PresetGeometry::GetPrimitiveType() const
{
	return this->m_geometry->GetPrimitiveType();
}

std::size_t PresetGeometry::GetPrimitiveSize() const
{
	return this->m_geometry->GetPrimitiveSize();
}

glm::ivec2 PresetGeometry::GetPrimitiveDimensions() const
{
	return this->m_geometry->GetPrimitiveDimensions();
}

Geometry::Interpolation PresetGeometry::GetInterpolationMode() const
{
	return this->m_geometry->GetInterpolationMode();
}

bool PresetGeometry::GetTesselationEnabled() const
{
	return this->m_geometry->GetTesselationEnabled();
}

std::string GetPresetGeometryType(PresetGeometry::GeometryType preset_geometry_type)
{
	switch (preset_geometry_type)
	{
	case PresetGeometry::GeometryType::Icosphere: return "Icosphere";
	case PresetGeometry::GeometryType::Plane: return "Plane";
	default: throw std::invalid_argument("Unknown preset geometry type: " + std::to_string(static_cast<int>(preset_geometry_type)));
	}
}
