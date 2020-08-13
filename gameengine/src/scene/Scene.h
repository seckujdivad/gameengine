#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <vector>
#include <unordered_set>
#include <string>
#include <tuple>

#include "model/Model.h"
#include "Camera.h"
#include "light/PointLight.h"
#include "model/Reflection.h"
#include "VisBox.h"
#include "OrientedBoundingBox.h"
#include "../render/Renderable.h"

typedef unsigned int TextureReference;

class Scene : public Nameable
{
private:
	//misc rendering attributes
	glm::vec3 m_light_ambient = glm::vec3(0.0f, 0.0f, 0.0f);
	glm::vec4 m_clear_colour = glm::vec4(1.0f, 1.0f, 1.0f, 1.0f);

	//skybox
	Scene* m_skybox_scene = nullptr;
	RenderTextureReference m_skybox_texture;

	//scene components - all are managed by the scene
	std::vector<Model*> m_models;
	std::vector<PointLight*> m_pointlights;
	std::vector<Reflection*> m_reflections;
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
	~Scene();

	void ManageChildren(bool manage);
	bool ChildrenAreManaged();

	void Add(Model* model);
	void Remove(Model* model);
	void RemoveModel(ModelReference reference);
	Model* GetModel(ModelReference reference);
	std::vector<Model*> GetModels();
	std::vector<Model*> GetVisibleModels(glm::dvec3 position, RenderMode mode);

	void Add(PointLight* pointlight);
	void Remove(PointLight* pointlight);
	std::vector<PointLight*> GetPointLights();

	void Add(Reflection* reflection);
	void Remove(Reflection* reflection);
	std::vector<Reflection*> GetReflections();

	void Add(VisBox* visbox);
	void Remove(VisBox* visbox);
	std::vector<VisBox*> GetVisBoxes();

	void RemoveCubemap(RenderTextureReference reference);
	std::tuple<Cubemap*, CubemapType> GetCubemap(RenderTextureReference reference);
	std::vector<std::tuple<Cubemap*, CubemapType>> GetCubemaps();

	void Add(OrientedBoundingBox obb);
	void Remove(OrientedBoundingBox obb);
	std::vector<OrientedBoundingBox> GetOBBApproximations();

	void SetSkyboxScene(Scene* scene);
	Scene* GetSkyboxScene();

	RenderTextureReference GetSkyboxTextureReference();

	void SetClearColour(glm::vec4 colour);
	glm::vec4 GetClearColour();

	void SetAmbientLight(glm::vec3 colour);
	glm::vec3 GetAmbientLight();

	ModelReference GetNewModelReference();
	RenderTextureReference GetNewRenderTextureReference();
	TextureReference GetNewTextureReference();
};
