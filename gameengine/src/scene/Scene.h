#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <vector>
#include <unordered_set>
#include <string>

#include "model/Model.h"
#include "Camera.h"
#include "light/PointLight.h"
#include "model/Reflection.h"
#include "VisBox.h"
#include "SceneApproximation.h"
#include "../render/Renderable.h"

typedef unsigned int TextureReference;

class Scene : public Nameable
{
private:
	//misc rendering attributes
	glm::vec3 m_light_ambient = glm::vec3(0.0f, 0.0f, 0.0f);
	glm::vec4 m_clear_colour = glm::vec4(1.0f, 1.0f, 1.0f, 1.0f);
	Scene* m_skybox_scene = nullptr;

	//scene components - all are managed by the scene
	std::vector<Model*> m_models;
	std::vector<PointLight*> m_pointlights;
	std::vector<Reflection*> m_reflections;
	std::vector<VisBox*> m_visboxes;

	bool m_manage_children = true;

	//manage (int) references - references are unique on a scene level, not any higher
	ModelReference m_reference_model = 0;
	CubemapReference m_reference_cubemap = 0;
	TextureReference m_reference_texture = 0;

public:
	Scene();
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

	void RemoveCubemap(CubemapReference reference);
	Cubemap* GetCubemap(CubemapReference reference);

	void SetSkyboxScene(Scene* scene);
	Scene* GetSkyboxScene();

	void SetClearColour(glm::vec4 colour);
	glm::vec4 GetClearColour();

	void SetAmbientLight(glm::vec3 colour);
	glm::vec3 GetAmbientLight();

	ModelReference GetNewModelReference();
	CubemapReference GetNewCubemapReference();
	TextureReference GetNewTextureReference();
};
