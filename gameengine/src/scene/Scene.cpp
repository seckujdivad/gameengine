#include "Scene.h"

#include <set>

#include "model/Model.h"
#include "light/PointLight.h"
#include "model/Reflection.h"
#include "Skybox.h"
#include "VisBox.h"

Scene::Scene() : Nameable()
{
}

Scene::~Scene()
{
	if (this->m_manage_children)
	{
		for (Model* model : this->m_models)
		{
			delete model;
		}

		for (PointLight* pointlight : this->m_pointlights)
		{
			delete pointlight;
		}

		for (Reflection* reflection : this->m_reflections)
		{
			delete reflection;
		}

		for (Skybox* skybox : this->m_skyboxes)
		{
			delete skybox;
		}

		for (VisBox* visbox : this->m_visboxes)
		{
			delete visbox;
		}
	}
}

void Scene::ManageChildren(bool manage)
{
	this->m_manage_children = manage;
}

bool Scene::ChildrenAreManaged() const
{
	return this->m_manage_children;
}

void Scene::Add(Model* model)
{
	this->m_models.push_back(model);
}

void Scene::Remove(Model* model)
{
	std::vector<Model*>::iterator it = std::find(this->m_models.begin(), this->m_models.end(), model);
	if (it != this->m_models.end())
	{
		this->m_models.erase(it);

		if (this->m_manage_children)
		{
			delete *it;
		}
	}

	for (std::tuple<Cubemap*, CubemapType> cubemap_data : this->GetCubemaps())
	{
		std::get<0>(cubemap_data)->RemoveStaticModel(model->GetReference());
		std::get<0>(cubemap_data)->RemoveDynamicModel(model->GetReference());
	}

	for (VisBox* visbox : this->GetVisBoxes())
	{
		visbox->RemoveMemberModel(model);
	}
}

void Scene::RemoveModel(ModelReference reference)
{
	this->Remove(this->GetModel(reference));
}

Model* Scene::GetModel(ModelReference reference) const
{
	for (Model* model : this->m_models)
	{
		if (model->GetReference() == reference)
		{
			return model;
		}
	}

	return nullptr;
}

Model* Scene::GetModel(std::string identifier) const
{
	for (Model* model : this->m_models)
	{
		if (model->GetIdentifier() == identifier)
		{
			return model;
		}
	}

	return nullptr;
}

std::vector<Model*> Scene::GetModels() const
{
	return this->m_models;
}

std::vector<Model*> Scene::GetModels(std::vector<ModelReference> references) const
{
	std::vector<Model*> models;
	for (ModelReference reference : references)
	{
		Model* model = this->GetModel(reference);
		if (model == nullptr)
		{
			throw std::invalid_argument("Model reference " + std::to_string(reference) + " is invalid");
		}
		else
		{
			models.push_back(model);
		}
	}
	return models;
}

void Scene::SetAmbientLight(glm::vec3 light_intensity)
{
	this->m_light_ambient = light_intensity;
}

glm::vec3 Scene::GetAmbientLight() const
{
	return this->m_light_ambient;
}

ModelReference Scene::GetNewModelReference()
{
	return this->m_reference_model++;
}

RenderTextureReference Scene::GetNewRenderTextureReference()
{
	return this->m_reference_render_texture++;
}

TextureReference Scene::GetNewTextureReference()
{
	return this->m_reference_texture++;
}

void Scene::SetClearColour(glm::vec4 colour)
{
	this->m_clear_colour = colour;
}

glm::vec4 Scene::GetClearColour() const
{
	return this->m_clear_colour;
}

std::vector<Model*> Scene::GetVisibleModels(glm::dvec3 position, RenderMode mode, std::vector<Model*> model_pool) const
{
	if ((mode == RenderMode::Normal) || (mode == RenderMode::Shadow))
	{
		std::set<Model*> visible_models;
		std::set<VisBox*> enclosed_visboxes;

		for (VisBox* visbox : this->m_visboxes)
		{
			if (visbox->PointInBounds(position))
			{
				enclosed_visboxes.insert(enclosed_visboxes.begin(), visbox);
			}
		}

		if (enclosed_visboxes.size() == 0) //player is outside of level, draw everything (for navigating back to the level if nothing else)
		{
			for (Model* model : this->m_models)
			{
				visible_models.insert(model);
			}
		}
		else
		{
			for (auto it = enclosed_visboxes.begin(); it != enclosed_visboxes.end(); it++)
			{
				std::set<Model*> locally_visible_models = (*it)->GetPotentiallyVisibleModels();
				visible_models.insert(locally_visible_models.begin(), locally_visible_models.end());
			}
		}

		std::vector<Model*> output;
		if (model_pool == this->GetModels())
		{
			output.assign(visible_models.begin(), visible_models.end());
		}
		else
		{
			std::set<Model*> output_set;

			std::set<Model*> model_pool_set;
			model_pool_set.insert(model_pool.begin(), model_pool.end());
			
			std::set_intersection(model_pool_set.begin(), model_pool_set.end(), visible_models.begin(), visible_models.end(), std::inserter(output_set, output_set.begin()));

			output.assign(output_set.begin(), output_set.end());
		}

		std::sort(output.begin(), output.end(), [position](Model* a, Model* b) mutable { return glm::length(a->GetPosition() - position) < glm::length(b->GetPosition() - position); }); //true means a should be moved forward and drawn earlier

		return output;
	}
	else if (mode == RenderMode::Wireframe)
	{
		return this->m_models;
	}
	else
	{
		throw std::runtime_error("Unsupported shading mode");
	}
}

void Scene::Add(PointLight* pointlight)
{
	this->m_pointlights.push_back(pointlight);
}

void Scene::Remove(PointLight* pointlight)
{
	std::vector<PointLight*>::iterator it = std::find(this->m_pointlights.begin(), this->m_pointlights.end(), pointlight);
	if (it != this->m_pointlights.end())
	{
		this->m_pointlights.erase(it);
		
		if (this->m_manage_children)
		{
			delete *it;
		}
	}
}

std::vector<PointLight*> Scene::GetPointLights() const
{
	return this->m_pointlights;
}

void Scene::Add(Reflection* reflection)
{
	this->m_reflections.push_back(reflection);
}

Reflection* Scene::GetReflection(std::string identifier) const
{
	for (Reflection* reflection : this->m_reflections)
	{
		if (reflection->GetIdentifier() == identifier)
		{
			return reflection;
		}
	}

	return nullptr;
}

void Scene::Remove(Reflection* reflection)
{
	std::vector<Reflection*>::iterator it = std::find(this->m_reflections.begin(), this->m_reflections.end(), reflection);
	if (it != this->m_reflections.end())
	{
		this->m_reflections.erase(it);
		
		if (this->m_manage_children)
		{
			delete *it;
		}
	}

	for (Model* model : this->GetModels())
	{
		Material& material = model->GetMaterial();

		for (int i = 0; i < static_cast<int>(material.reflections.size()); i++)
		{
			std::tuple<Reflection*, ReflectionMode> reflection_data = material.reflections.at(i);

			if (std::get<0>(reflection_data) == reflection)
			{
				material.reflections.erase(material.reflections.begin(), material.reflections.begin() + i);
				i--;
			}
		}
	}
}

std::vector<Reflection*> Scene::GetReflections() const
{
	return this->m_reflections;
}

void Scene::Add(Skybox* skybox)
{
	this->m_skyboxes.push_back(skybox);
}

Skybox* Scene::GetSkybox(std::string identifier) const
{
	for (Skybox* skybox : this->m_skyboxes)
	{
		if (skybox->GetIdentifier() == identifier)
		{
			return skybox;
		}
	}

	return nullptr;
}

void Scene::Remove(Skybox* skybox)
{
	std::vector<Skybox*>::iterator it = std::find(this->m_skyboxes.begin(), this->m_skyboxes.end(), skybox);
	if (it != this->m_skyboxes.end())
	{
		this->m_skyboxes.erase(it);

		if (this->m_manage_children)
		{
			delete* it;
		}
	}

	for (Model* model : this->GetModels())
	{
		if (model->GetSkybox() == skybox)
		{
			model->SetSkybox(nullptr);
		}
	}
}

std::vector<Skybox*> Scene::GetSkyboxes() const
{
	return this->m_skyboxes;
}

void Scene::Add(VisBox* visbox)
{
	this->m_visboxes.push_back(visbox);
}

VisBox* Scene::GetVisBox(std::string identifier) const
{
	for (VisBox* visbox : this->m_visboxes)
	{
		if (visbox->GetIdentifier() == identifier)
		{
			return visbox;
		}
	}

	return nullptr;
}

void Scene::Remove(VisBox* visbox)
{
	std::vector<VisBox*>::iterator it = std::find(this->m_visboxes.begin(), this->m_visboxes.end(), visbox);
	if (it != this->m_visboxes.end())
	{
		this->m_visboxes.erase(it);
		
		if (this->m_manage_children)
		{
			delete *it;
		}
	}
}

std::vector<VisBox*> Scene::GetVisBoxes() const
{
	return this->m_visboxes;
}

void Scene::RemoveCubemap(RenderTextureReference reference)
{
	{
		std::vector<Reflection*>::iterator result = this->m_reflections.end();
		for (std::vector<Reflection*>::iterator it = this->m_reflections.begin(); it != this->m_reflections.end(); it++)
		{
			if ((*it)->GetReference() == reference)
			{
				result = it;
			}
		}

		if (result != this->m_reflections.end())
		{
			this->Remove(*result);
		}
	}

	{
		std::vector<PointLight*>::iterator result = this->m_pointlights.end();
		for (std::vector<PointLight*>::iterator it = this->m_pointlights.begin(); it != this->m_pointlights.end(); it++)
		{
			if ((*it)->GetReference() == reference)
			{
				result = it;
			}
		}

		if (result != this->m_pointlights.end())
		{
			this->Remove(*result);
		}
	}

	{
		std::vector<Skybox*>::iterator result = this->m_skyboxes.end();
		for (std::vector<Skybox*>::iterator it = this->m_skyboxes.begin(); it != this->m_skyboxes.end(); it++)
		{
			if ((*it)->GetReference() == reference)
			{
				result = it;
			}
		}

		if (result != this->m_skyboxes.end())
		{
			this->Remove(*result);
		}
	}
}

std::tuple<Cubemap*, CubemapType> Scene::GetCubemap(RenderTextureReference reference) const
{
	for (Reflection* reflection : this->m_reflections)
	{
		if (reflection->GetReference() == reference)
		{
			return { reflection, CubemapType::Reflection };
		}
	}

	for (PointLight* point_light : this->m_pointlights)
	{
		if (point_light->GetReference() == reference)
		{
			return { point_light, CubemapType::Pointlight };
		}
	}

	for (Skybox* skybox : this->m_skyboxes)
	{
		if (skybox->GetReference() == reference)
		{
			return { skybox, CubemapType::Skybox };
		}
	}

	return { nullptr, CubemapType::None };
}

std::vector<std::tuple<Cubemap*, CubemapType>> Scene::GetCubemaps() const
{
	std::vector<std::tuple<Cubemap*, CubemapType>> cubemaps;
	for (auto it = this->m_reflections.begin(); it != this->m_reflections.end(); it++)
	{
		cubemaps.push_back({ *it, CubemapType::Reflection });
	}

	for (auto it = this->m_pointlights.begin(); it != this->m_pointlights.end(); it++)
	{
		cubemaps.push_back({ *it, CubemapType::Pointlight });
	}

	for (auto it = this->m_skyboxes.begin(); it != this->m_skyboxes.end(); it++)
	{
		cubemaps.push_back({ *it, CubemapType::Skybox });
	}

	return cubemaps;
}

void Scene::Add(OrientedBoundingBox obb)
{
	this->m_approximations.push_back(obb);
}

void Scene::Remove(OrientedBoundingBox obb)
{
	this->m_approximations.erase(std::find(this->m_approximations.begin(), this->m_approximations.end(), obb));
}

std::vector<OrientedBoundingBox> Scene::GetOBBApproximations() const
{
	return this->m_approximations;
}
