#pragma once

#include <vector>
#include <memory>

#include <glm/glm.hpp>

#include "../Positionable.h"
#include "../Rotatable.h"
#include "../Scalable.h"
#include "../Nameable.h"
#include "Material.h"
#include "../Referenceable.h"
#include "../LocalTexture.h"
#include "../SceneChild.h"

class Scene;
class Skybox;
class Geometry;

class Model : public Positionable, public Rotatable, public Scalable, public Nameable, public Referenceable<ModelReference>, public SceneChild
{
private:
	std::vector<std::shared_ptr<Geometry>> m_geometry;
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
	Model(ModelReference reference, std::vector<std::shared_ptr<Geometry>> geometry, Scene* scene = nullptr);

	Material& GetMaterial();

	std::vector<std::shared_ptr<Geometry>> GetGeometry();

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

	void SetSkybox(Skybox* skybox);
	Skybox* GetSkybox() const;
};
