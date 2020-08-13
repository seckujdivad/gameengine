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

bool operator==(const ModelGeometry& first, const ModelGeometry& second);
bool operator!=(const ModelGeometry& first, const ModelGeometry& second);
bool operator==(const Face& first, const Face& second);
bool operator!=(const Face& first, const Face& second);

void MergeVertices(ModelGeometry& geometry, double threshold = 0.0);
void InvertNormals(ModelGeometry& geometry);

std::vector<double> GetTriangles(const ModelGeometry& geometry, bool only_geometry = false);

std::vector<GLfloat> DoubleToSinglePrecision(std::vector<double> vec); //utility method for changing the precision of GetTriangles before feeding to the GPU

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
	LocalTexture m_texture_skybox_mask = LocalTexture(0);

	//wireframe colour
	glm::vec3 m_wireframe_colour = glm::vec3(0.0f);

public:
	Model(ModelReference reference, ModelGeometry geometry, Scene* scene = nullptr);

	std::vector<std::vector<double>> GetTriFans(); //not implemented
	std::vector<std::vector<double>> GetTriStrips(); //not implemented
	std::vector<double> GetTriangles(bool only_geometry = false);

	Material& GetMaterial();

	ModelGeometry GetGeometry();

	LocalTexture& GetColourTexture();
	LocalTexture& GetReflectionTexture();
	LocalTexture& GetSpecularTexture();
	LocalTexture& GetNormalTexture();
	LocalTexture& GetSkyboxMaskTexture();

	void SetWireframeColour(glm::vec3 colour);
	void SetWireframeColourSelected();
	void SetWireframeColourUnselected();
	glm::vec3 GetWireframeColour();

	static constexpr int GetValuesPerVert();
};