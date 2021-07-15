#include "Scene.h"

#include <set>
#include <unordered_set>
#include <stdexcept>
#include <iterator>
#include <algorithm>

#include "Cubemap.h"
#include "model/Model.h"
#include "light/PointLight.h"
#include "model/Reflection.h"
#include "Skybox.h"
#include "VisBox.h"

Scene::Scene(bool ensure_drawable) : Nameable()
{
	if (ensure_drawable)
	{
		this->EnsureDrawable();
	}
}

void Scene::Add(std::shared_ptr<Model> model)
{
	this->m_models.push_back(model);
}

void Scene::Remove(std::shared_ptr<Model> model)
{
	std::vector<std::shared_ptr<Model>>::iterator it = std::find(this->m_models.begin(), this->m_models.end(), model);
	if (it != this->m_models.end())
	{
		this->m_models.erase(it);
	}

	for (const std::shared_ptr<Cubemap>& cubemaps : this->GetCubemaps())
	{
		cubemaps->RemoveStaticModel(model->GetReference());
		cubemaps->RemoveDynamicModel(model->GetReference());
	}

	for (const std::shared_ptr<VisBox>& visbox : this->GetVisBoxes())
	{
		visbox->RemoveMemberModel(model);
	}
}

void Scene::RemoveModel(ModelReference reference)
{
	std::optional<std::shared_ptr<Model>> model = this->GetModel(reference);
	if (model.has_value())
	{
		this->Remove(model.value());
	}
}

std::optional<std::shared_ptr<Model>> Scene::GetModel(ModelReference reference) const
{
	for (const std::shared_ptr<Model>& model : this->m_models)
	{
		if (model->GetReference() == reference)
		{
			return model;
		}
	}

	return std::optional<std::shared_ptr<Model>>();
}

std::optional<std::shared_ptr<Model>> Scene::GetModel(std::string identifier) const
{
	for (const std::shared_ptr<Model>& model : this->m_models)
	{
		if (model->GetIdentifier() == identifier)
		{
			return model;
		}
	}

	return std::optional<std::shared_ptr<Model>>();
}

const std::vector<std::shared_ptr<Model>>& Scene::GetModels() const
{
	return this->m_models;
}

std::vector<std::shared_ptr<Model>> Scene::GetModels(std::vector<ModelReference> references) const
{
	std::vector<std::shared_ptr<Model>> models;
	for (ModelReference reference : references)
	{
		std::optional<std::shared_ptr<Model>> model = this->GetModel(reference);
		if (model.has_value())
		{
			models.push_back(model.value());
		}
		else
		{
			throw std::invalid_argument("Model reference " + std::to_string(reference) + " is invalid");
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

std::mutex& Scene::GetMutex()
{
	return this->m_scene_mutex;
}

const std::mutex& Scene::GetMutex() const
{
	return this->m_scene_mutex;
}

void Scene::ClearObjects()
{
	if (!this->IsCleared())
	{
		this->m_models.clear();
		this->m_pointlights.clear();
		this->m_reflections.clear();
		this->m_skyboxes.clear();
		this->m_visboxes.clear();
		this->m_approximations.clear();
	}
}

bool Scene::IsCleared() const
{
	return this->m_models.size() == 0
		&& this->m_pointlights.size() == 0
		&& this->m_reflections.size() == 0
		&& this->m_skyboxes.size() == 0
		&& this->m_visboxes.size() == 0
		&& this->m_approximations.size() == 0;
}

void Scene::EnsureDrawable()
{
	this->ClearObjects();
	this->Add(std::make_shared<PointLight>(this->GetNewRenderTextureReference()));
	this->Add(std::make_shared<Reflection>(this->GetNewRenderTextureReference()));
	this->Add(OrientedBoundingBox());
}

void Scene::SetClearColour(glm::vec4 colour)
{
	this->m_clear_colour = colour;
}

glm::vec4 Scene::GetClearColour() const
{
	return this->m_clear_colour;
}

std::vector<Model*> Scene::GetVisibleModels(glm::dvec3 position, RenderTargetMode mode, std::vector<Model*> model_pool) const
{
	if ((mode == RenderTargetMode::Normal_DepthOnly) || (mode == RenderTargetMode::Normal_Draw) || (mode == RenderTargetMode::Shadow) || (mode == RenderTargetMode::Textured))
	{
		std::set<Model*> visible_models;
		std::unordered_set<VisBox*> enclosed_visboxes;

		for (const std::shared_ptr<VisBox> visbox : this->m_visboxes)
		{
			if (visbox->PointInBounds(position))
			{
				enclosed_visboxes.insert(enclosed_visboxes.begin(), visbox.get());
			}
		}

		if (enclosed_visboxes.size() == 0) //player is outside of level, draw everything (for navigating back to the level if nothing else)
		{
			for (const std::shared_ptr<Model>& model : this->m_models)
			{
				visible_models.insert(model.get());
			}
		}
		else
		{
			for (VisBox* visbox : enclosed_visboxes)
			{
				std::set<std::shared_ptr<Model>> locally_visible_models = visbox->GetPotentiallyVisibleModels();
				for (const std::shared_ptr<Model>& model : locally_visible_models)
				{
					visible_models.insert(model.get());
				}
			}
		}

		std::vector<Model*> output;
		std::unordered_set<Model*> output_set;

		std::set<Model*> model_pool_set;
		model_pool_set.insert(model_pool.begin(), model_pool.end());
			
		std::set_intersection(model_pool_set.begin(), model_pool_set.end(), visible_models.begin(), visible_models.end(), std::inserter(output_set, output_set.begin()));

		output.assign(output_set.begin(), output_set.end());
		return output;
	}
	else if (mode == RenderTargetMode::Wireframe)
	{
		return model_pool;
	}
	else
	{
		throw std::runtime_error("Unsupported shading mode");
	}
}

void Scene::Add(std::shared_ptr<PointLight> pointlight)
{
	this->m_pointlights.push_back(pointlight);
}

std::optional<std::shared_ptr<PointLight>> Scene::GetPointLight(RenderTextureReference reference) const
{
	for (const std::shared_ptr<PointLight>& pointlight : this->m_pointlights)
	{
		if (pointlight->GetReference() == reference)
		{
			return pointlight;
		}
	}
	return std::optional<std::shared_ptr<PointLight>>();
}

std::optional<std::shared_ptr<PointLight>> Scene::GetPointLight(std::string identifier) const
{
	for (const std::shared_ptr<PointLight>& pointlight : this->m_pointlights)
	{
		if (pointlight->GetIdentifier() == identifier)
		{
			return pointlight;
		}
	}
	return std::optional<std::shared_ptr<PointLight>>();
}

void Scene::Remove(std::shared_ptr<PointLight> pointlight)
{
	std::vector<std::shared_ptr<PointLight>>::iterator it = std::find(this->m_pointlights.begin(), this->m_pointlights.end(), pointlight);
	if (it != this->m_pointlights.end())
	{
		this->m_pointlights.erase(it);
	}
}

const std::vector<std::shared_ptr<PointLight>>& Scene::GetPointLights() const
{
	return this->m_pointlights;
}

void Scene::Add(std::shared_ptr<Reflection> reflection)
{
	this->m_reflections.push_back(reflection);
}

std::optional<std::shared_ptr<Reflection>> Scene::GetReflection(RenderTextureReference reference) const
{
	for (const std::shared_ptr<Reflection>& reflection : this->m_reflections)
	{
		if (reflection->GetReference() == reference)
		{
			return reflection;
		}
	}

	return std::optional<std::shared_ptr<Reflection>>();
}

std::optional<std::shared_ptr<Reflection>> Scene::GetReflection(std::string identifier) const
{
	for (const std::shared_ptr<Reflection>& reflection : this->m_reflections)
	{
		if (reflection->GetIdentifier() == identifier)
		{
			return reflection;
		}
	}

	return std::optional<std::shared_ptr<Reflection>>();
}

void Scene::Remove(std::shared_ptr<Reflection> reflection)
{
	std::vector<std::shared_ptr<Reflection>>::iterator it = std::find(this->m_reflections.begin(), this->m_reflections.end(), reflection);
	if (it != this->m_reflections.end())
	{
		this->m_reflections.erase(it);
	}

	for (const std::shared_ptr<Model>& model : this->GetModels())
	{
		Material& material = model->GetMaterial();

		for (int i = 0; i < static_cast<int>(material.reflections.size()); i++)
		{
			std::tuple<std::shared_ptr<Reflection>, ReflectionMode> reflection_data = material.reflections.at(i);

			if (std::get<0>(reflection_data) == reflection)
			{
				material.reflections.erase(material.reflections.begin(), material.reflections.begin() + i);
				i--;
			}
		}
	}
}

const std::vector<std::shared_ptr<Reflection>>& Scene::GetReflections() const
{
	return this->m_reflections;
}

void Scene::Add(std::shared_ptr<Skybox> skybox)
{
	this->m_skyboxes.push_back(skybox);
}

std::optional<std::shared_ptr<Skybox>> Scene::GetSkybox(RenderTextureReference reference) const
{
	for (const std::shared_ptr<Skybox>& skybox : this->m_skyboxes)
	{
		if (skybox->GetReference() == reference)
		{
			return skybox;
		}
	}

	return std::optional<std::shared_ptr<Skybox>>();
}

std::optional<std::shared_ptr<Skybox>> Scene::GetSkybox(std::string identifier) const
{
	for (const std::shared_ptr<Skybox>& skybox : this->m_skyboxes)
	{
		if (skybox->GetIdentifier() == identifier)
		{
			return skybox;
		}
	}

	return std::optional<std::shared_ptr<Skybox>>();
}

void Scene::Remove(std::shared_ptr<Skybox> skybox)
{
	std::vector<std::shared_ptr<Skybox>>::iterator it = std::find(this->m_skyboxes.begin(), this->m_skyboxes.end(), skybox);
	if (it != this->m_skyboxes.end())
	{
		this->m_skyboxes.erase(it);
	}

	for (const std::shared_ptr<Model>& model : this->GetModels())
	{
		if (model->GetSkybox() == skybox)
		{
			model->SetSkybox(nullptr);
		}
	}
}

const std::vector<std::shared_ptr<Skybox>>& Scene::GetSkyboxes() const
{
	return this->m_skyboxes;
}

void Scene::Add(std::shared_ptr<VisBox> visbox)
{
	this->m_visboxes.push_back(visbox);
}

std::optional<std::shared_ptr<VisBox>> Scene::GetVisBox(std::string identifier) const
{
	for (const std::shared_ptr<VisBox>& visbox : this->m_visboxes)
	{
		if (visbox->GetIdentifier() == identifier)
		{
			return visbox;
		}
	}

	return std::optional<std::shared_ptr<VisBox>>();
}

void Scene::Remove(std::shared_ptr<VisBox> visbox)
{
	std::vector<std::shared_ptr<VisBox>>::iterator it = std::find(this->m_visboxes.begin(), this->m_visboxes.end(), visbox);
	if (it != this->m_visboxes.end())
	{
		this->m_visboxes.erase(it);
	}

	for (const std::shared_ptr<VisBox>& second_visbox : this->GetVisBoxes())
	{
		second_visbox->RemovePotentiallyVisible(visbox.get());
	}
}

const std::vector<std::shared_ptr<VisBox>>& Scene::GetVisBoxes() const
{
	return this->m_visboxes;
}

void Scene::RemoveCubemap(RenderTextureReference reference)
{
	{
		std::vector<std::shared_ptr<Reflection>>::iterator result = this->m_reflections.end();
		for (std::vector<std::shared_ptr<Reflection>>::iterator it = this->m_reflections.begin(); it != this->m_reflections.end(); it++)
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
		std::vector<std::shared_ptr<PointLight>>::iterator result = this->m_pointlights.end();
		for (std::vector<std::shared_ptr<PointLight>>::iterator it = this->m_pointlights.begin(); it != this->m_pointlights.end(); it++)
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
		std::vector<std::shared_ptr<Skybox>>::iterator result = this->m_skyboxes.end();
		for (std::vector<std::shared_ptr<Skybox>>::iterator it = this->m_skyboxes.begin(); it != this->m_skyboxes.end(); it++)
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

std::optional<std::shared_ptr<Cubemap>> Scene::GetCubemap(RenderTextureReference reference) const
{
	for (const std::shared_ptr<Reflection>& reflection : this->m_reflections)
	{
		if (reflection->GetReference() == reference)
		{
			return reflection;
		}
	}

	for (const std::shared_ptr<PointLight>& point_light : this->m_pointlights)
	{
		if (point_light->GetReference() == reference)
		{
			return point_light;
		}
	}

	for (const std::shared_ptr<Skybox>& skybox : this->m_skyboxes)
	{
		if (skybox->GetReference() == reference)
		{
			return skybox;
		}
	}

	return std::optional<std::shared_ptr<Cubemap>>();
}

std::vector<std::shared_ptr<Cubemap>> Scene::GetCubemaps() const
{
	std::vector<std::shared_ptr<Cubemap>> cubemaps;
	for (const std::shared_ptr<Reflection>& reflection : this->m_reflections)
	{
		cubemaps.push_back(reflection);
	}

	for (const std::shared_ptr<PointLight>& pointlight : this->m_pointlights)
	{
		cubemaps.push_back(pointlight);
	}

	for (const std::shared_ptr<Skybox>& skybox : this->m_skyboxes)
	{
		cubemaps.push_back(skybox);
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

const std::vector<OrientedBoundingBox>& Scene::GetOBBApproximations() const
{
	return this->m_approximations;
}
