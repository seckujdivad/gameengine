#pragma once

#include <vector>
#include <string>
#include <tuple>
#include <memory>
#include <optional>
#include <mutex>
#include <unordered_map>

#include "OrientedBoundingBox.h"
#include "Nameable.h"
#include "Referenceable.h"
#include "../render/rendertarget/target/RenderTargetMode.h"

class Model;
class PointLight;
class Reflection;
class Skybox;
class VisBox;
class Scene;
class Cubemap;

class Scene : public Nameable
{
private:
	//misc rendering attributes
	glm::vec3 m_light_ambient = glm::vec3(0.0f, 0.0f, 0.0f);
	glm::vec4 m_clear_colour = glm::vec4(1.0f, 1.0f, 1.0f, 0.0f);

	//scene components - all are managed by the scene
	std::unordered_map<ModelReference, std::shared_ptr<Model>> m_models;
	std::vector<std::shared_ptr<PointLight>> m_pointlights;
	std::vector<std::shared_ptr<Reflection>> m_reflections;
	std::vector<std::shared_ptr<Skybox>> m_skyboxes;
	std::vector<std::shared_ptr<VisBox>> m_visboxes;
	std::vector<OrientedBoundingBox> m_approximations;

	//manage (int) references - references are unique on a scene level, not any higher
	ModelReference m_reference_model = 0;
	RenderTextureReference m_reference_render_texture = 0;
	TextureReference m_reference_texture = 0;

	std::mutex m_scene_mutex; //allows one thread to take exclusive ownership of the Scene when rendering or doing operations that can't take place while rendering - the scene itself won't bother trying to acquire this mutex

public:
	Scene();

	void Add(std::shared_ptr<Model> model);
	void Remove(std::shared_ptr<Model> model);
	void RemoveModel(ModelReference reference);
	std::optional<std::shared_ptr<Model>> GetModel(ModelReference reference) const;
	std::optional<std::shared_ptr<Model>> GetModel(std::string identifier) const;
	std::vector<std::shared_ptr<Model>> GetModels() const;
	std::vector<std::shared_ptr<Model>> GetModels(std::vector<ModelReference> references) const;
	std::vector<Model*> GetVisibleModels(glm::dvec3 position, RenderTargetMode mode, std::vector<Model*> model_pool) const; //the result should not be stored (as there are no guarantees for how long it will be valid). Therefore, there should be no ownership of the results of this method
	std::vector<Model*> GetRawModelPointers() const;

	void Add(std::shared_ptr<PointLight> pointlight);
	std::optional<std::shared_ptr<PointLight>> GetPointLight(RenderTextureReference reference) const;
	std::optional<std::shared_ptr<PointLight>> GetPointLight(std::string identifier) const;
	void Remove(std::shared_ptr<PointLight> pointlight);
	const std::vector<std::shared_ptr<PointLight>>& GetPointLights() const;

	void Add(std::shared_ptr<Reflection> reflection);
	std::optional<std::shared_ptr<Reflection>> GetReflection(RenderTextureReference reference) const;
	std::optional<std::shared_ptr<Reflection>> GetReflection(std::string identifier) const;
	void Remove(std::shared_ptr<Reflection> reflection);
	const std::vector<std::shared_ptr<Reflection>>& GetReflections() const;

	void Add(std::shared_ptr<Skybox> skybox);
	std::optional<std::shared_ptr<Skybox>> GetSkybox(RenderTextureReference reference) const;
	std::optional<std::shared_ptr<Skybox>> GetSkybox(std::string identifier) const;
	void Remove(std::shared_ptr<Skybox> skybox);
	const std::vector<std::shared_ptr<Skybox>>& GetSkyboxes() const;

	void RemoveCubemap(RenderTextureReference reference);
	std::optional<std::shared_ptr<Cubemap>> GetCubemap(RenderTextureReference reference) const;
	std::vector<std::shared_ptr<Cubemap>> GetCubemaps() const;

	void Add(std::shared_ptr<VisBox> visbox);
	std::optional<std::shared_ptr<VisBox>> GetVisBox(std::string identifier) const;
	void Remove(std::shared_ptr<VisBox> visbox);
	const std::vector<std::shared_ptr<VisBox>>& GetVisBoxes() const;

	void Add(OrientedBoundingBox obb);
	void Remove(OrientedBoundingBox obb);
	const std::vector<OrientedBoundingBox>& GetOBBApproximations() const;

	void SetClearColour(glm::vec4 colour);
	glm::vec4 GetClearColour() const;

	void SetAmbientLight(glm::vec3 colour);
	glm::vec3 GetAmbientLight() const;

	ModelReference GetNewModelReference();
	RenderTextureReference GetNewRenderTextureReference();
	TextureReference GetNewTextureReference();

	std::mutex& GetMutex();
	const std::mutex& GetMutex() const;

	void ClearObjects(); //clear all scene objects while preserving UIDs
	bool IsCleared() const;
};