#pragma once

#include <vector>
#include <memory>
#include <mutex>

#include <glm/glm.hpp>

#include "Material.h"
#include "../Nameable.h"
#include "../Referenceable.h"
#include "../SceneChild.h"
#include "../texture/Texture.h"
#include "../transformations/Positionable.h"
#include "../transformations/Rotatable.h"
#include "../transformations/Scalable.h"

class Scene;
class Skybox;
class Geometry;

class Model : public Positionable, public Rotatable, public Scalable, public Nameable, public Referenceable<ModelReference>, public SceneChild
{
private:
	std::mutex m_geometry_lock;
	std::vector<std::shared_ptr<Geometry>> m_geometry;

	Material m_material;

	//textures - all need to be replaced before they can be used
	Texture m_texture_colour = Texture(0);
	Texture m_texture_reflection = Texture(0);
	Texture m_texture_specular = Texture(0);
	Texture m_texture_normal = Texture(0);
	Texture m_texture_skybox_mask = Texture(0);
	Texture m_texture_displacement = Texture(0);

	//wireframe colour
	int m_wireframe_colours_index = 0;
	std::vector<glm::vec3> m_wireframe_colours = { glm::vec3(0.0f), glm::vec3(1.0f, 0.75f, 0.0f) };

	//skybox
	std::shared_ptr<Skybox> m_skybox = nullptr;

public:
	Model(ModelReference reference, std::vector<std::shared_ptr<Geometry>> geometry, Scene* scene = nullptr);

	Material& GetMaterial();

	std::vector<std::shared_ptr<Geometry>> GetGeometry();

	Texture& GetColourTexture();
	Texture& GetReflectionTexture();
	Texture& GetSpecularTexture();
	Texture& GetNormalTexture();
	Texture& GetSkyboxMaskTexture();
	Texture& GetDisplacementTexture();

	void SetWireframeColours(std::vector<glm::vec3> colours);
	void SetCurrentWireframeIndex(int index);
	void SetCurrentWireframeColour(glm::vec3 colour);
	glm::vec3 GetCurrentWireframeColour() const;

	void SetSkybox(std::shared_ptr<Skybox> skybox);
	std::shared_ptr<Skybox> GetSkybox() const;

	std::mutex& GetGeometryMutex();
	const std::mutex& GetGeometryMutex() const;
};
