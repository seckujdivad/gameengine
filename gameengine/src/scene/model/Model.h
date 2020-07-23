#pragma once

#include <wx/wxprec.h>
#include "../../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <string>
#include <vector>
#include <tuple>
#include <string>
#include <cmath>

#include "../Positionable.h"
#include "../Rotatable.h"
#include "../Scalable.h"
#include "../Nameable.h"
#include "Material.h"
#include "../Referenceable.h"
#include "../LocalTexture.h"

struct Face
{
	std::vector<int> vertices;
	std::vector<glm::dvec2> uv;
	glm::dvec3 normal;
};

struct ModelGeometry
{
	std::vector<Face> faces;
	std::vector<glm::dvec3> vertices;
};

void MergeVertices(ModelGeometry& geometry, double threshold = 0.0);
void InvertNormals(ModelGeometry& geometry);

class Model : public Positionable, public Rotatable, public Scalable, public Nameable, public Referenceable<ModelReference>
{
private:
	Scene* m_scene;

	ModelGeometry m_geometry;
	Material m_material;

	//textures - all need to be replaced before they can be used
	LocalTexture m_texture_colour = LocalTexture(0);
	LocalTexture m_texture_reflection = LocalTexture(0);
	LocalTexture m_texture_specular = LocalTexture(0);
	LocalTexture m_texture_normal = LocalTexture(0);

public:
	Model(ModelReference reference, ModelGeometry geometry, Scene* scene);

	std::vector<std::vector<double>> GetTriFans(); //not implemented
	std::vector<std::vector<double>> GetTriStrips(); //not implemented
	std::vector<double> GetTriangles();

	Material& GetMaterial();

	ModelGeometry GetGeometry();

	LocalTexture& GetColourTexture();
	LocalTexture& GetReflectionTexture();
	LocalTexture& GetSpecularTexture();
	LocalTexture& GetNormalTexture();
};