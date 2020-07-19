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
#include "../../EventManager.h"
#include "../../EventEmitter.h"

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

class Model : public Positionable, public Rotatable, public Scalable, public virtual EventEmitter, public Nameable
{
private:
	ModelGeometry m_geometry;

	Material m_material;

public:
	Model(EventManager* evtman, ModelGeometry geometry);
	Model(Model& copy_from);
	~Model();

	std::vector<std::vector<double>> GetTriFans(); //not implemented
	std::vector<std::vector<double>> GetTriStrips(); //not implemented
	std::vector<double> GetTriangles();

	void SetMaterial(Material material);
	Material GetMaterial();

	ModelGeometry GetGeometry();

#pragma warning(disable: 4250)
	using Nameable::GetIdentifier;
};
#pragma warning(default: 4250)