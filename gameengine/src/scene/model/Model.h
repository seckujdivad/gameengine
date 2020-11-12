#pragma once

#include "../../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <string>
#include <vector>
#include <tuple>

#include "../Positionable.h"
#include "../Rotatable.h"
#include "../Scalable.h"
#include "../Nameable.h"
#include "Material.h"
#include "../Referenceable.h"
#include "../LocalTexture.h"

class Scene;
class Skybox;

struct Face
{
	std::vector<int> vertices;
	std::vector<glm::dvec2> uv;
	glm::dvec3 normal = glm::dvec3(1.0, 0.0, 0.0);
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
	LocalTexture m_texture_displacement = LocalTexture(0);

	//wireframe colour
	int m_wireframe_colours_index = 0;
	std::vector<glm::vec3> m_wireframe_colours = { glm::vec3(0.0f), glm::vec3(1.0f, 0.75f, 0.0f) };

	//skybox
	Skybox* m_skybox = nullptr;

public:
	Model(ModelReference reference, ModelGeometry geometry, Scene* scene = nullptr);

	std::vector<std::vector<double>> GetTriFans() const; //not implemented
	std::vector<std::vector<double>> GetTriStrips() const; //not implemented
	std::vector<double> GetTriangles(bool only_geometry = false) const;

	Material& GetMaterial();

	const ModelGeometry& GetGeometry() const;

	LocalTexture& GetColourTexture();
	LocalTexture& GetReflectionTexture();
	LocalTexture& GetSpecularTexture();
	LocalTexture& GetNormalTexture();
	LocalTexture& GetSkyboxMaskTexture();
	LocalTexture& GetDisplacementTexture();

	void SetWireframeColours(std::vector<glm::vec3> colours);
	void SetCurrentWireframeIndex(int index);
	void SetCurrentWireframeColour(glm::vec3 colour);
	glm::vec3 GetCurrentWireframeColour() const;

	static constexpr int GetValuesPerVert() { return 14; };

	void SetSkybox(Skybox* skybox);
	Skybox* GetSkybox() const;
};
