#pragma once

#include <vector>
#include <string>
#include <tuple>

#include "OrientedBoundingBox.h"
#include "Nameable.h"
#include "Referenceable.h"
#include "../render/rendertarget/RenderTargetMode.h"
#include "Cubemap.h"

class Model;
class PointLight;
class Reflection;
class Skybox;
class VisBox;
class Scene;

class Scene : public Nameable
{
private:
	//misc rendering attributes
	glm::vec3 m_light_ambient = glm::vec3(0.0f, 0.0f, 0.0f);
	glm::vec4 m_clear_colour = glm::vec4(1.0f, 1.0f, 1.0f, 0.0f);

	//scene components - all are managed by the scene
	std::vector<Model*> m_models;
	std::vector<PointLight*> m_pointlights;
	std::vector<Reflection*> m_reflections;
	std::vector<Skybox*> m_skyboxes;
	std::vector<VisBox*> m_visboxes;
	std::vector<OrientedBoundingBox> m_approximations;

	bool m_manage_children = true;

	//manage (int) references - references are unique on a scene level, not any higher
	ModelReference m_reference_model = 0;
	RenderTextureReference m_reference_render_texture = 0;
	TextureReference m_reference_texture = 0;

public:
	Scene();
	Scene(const Scene&) = delete;
	Scene& operator=(const Scene&) = delete;
	Scene(Scene&&) = delete;
	Scene* operator=(Scene&&) = delete;
	~Scene();

	void ManageChildren(bool manage);
	bool ChildrenAreManaged() const;

	void Add(Model* model);
	void Remove(Model* model);
	void RemoveModel(ModelReference reference);
	Model* GetModel(ModelReference reference) const;
	Model* GetModel(std::string identifier) const;
	std::vector<Model*> GetModels() const;
	std::vector<Model*> GetModels(std::vector<ModelReference> references) const;
	std::vector<Model*> GetVisibleModels(glm::dvec3 position, RenderTargetMode mode, std::vector<Model*> model_pool) const;

	void Add(PointLight* pointlight);
	void Remove(PointLight* pointlight);
	std::vector<PointLight*> GetPointLights() const;

	void Add(Reflection* reflection);
	Reflection* GetReflection(std::string identifier) const;
	void Remove(Reflection* reflection);
	std::vector<Reflection*> GetReflections() const;

	void Add(Skybox* skybox);
	Skybox* GetSkybox(std::string identifier) const;
	void Remove(Skybox* skybox);
	std::vector<Skybox*> GetSkyboxes() const;

	void RemoveCubemap(RenderTextureReference reference);
	std::tuple<Cubemap*, CubemapType> GetCubemap(RenderTextureReference reference) const;
	std::vector<std::tuple<Cubemap*, CubemapType>> GetCubemaps() const;

	void Add(VisBox* visbox);
	VisBox* GetVisBox(std::string identifier) const;
	void Remove(VisBox* visbox);
	std::vector<VisBox*> GetVisBoxes() const;

	void Add(OrientedBoundingBox obb);
	void Remove(OrientedBoundingBox obb);
	std::vector<OrientedBoundingBox> GetOBBApproximations() const;

	void SetClearColour(glm::vec4 colour);
	glm::vec4 GetClearColour() const;

	void SetAmbientLight(glm::vec3 colour);
	glm::vec3 GetAmbientLight() const;

	ModelReference GetNewModelReference();
	RenderTextureReference GetNewRenderTextureReference();
	TextureReference GetNewTextureReference();
};
