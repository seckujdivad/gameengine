#include <wx/wxprec.h>
#include "Scene.h"

Scene::Scene() : Nameable()
{
}

Scene::~Scene()
{
	if (this->m_manage_children)
	{
		for (int i = 0; this->m_models.size(); i++) { delete this->m_models.at(i); };
		for (int i = 0; this->m_pointlights.size(); i++) { delete this->m_pointlights.at(i); };
		for (int i = 0; this->m_reflections.size(); i++) { delete this->m_reflections.at(i); };
		for (int i = 0; this->m_visboxes.size(); i++) { delete this->m_visboxes.at(i); };

		if (this->m_skybox_scene != nullptr)
		{
			delete this->m_skybox_scene;
		}
	}
}

void Scene::ManageChildren(bool manage)
{
	this->m_manage_children = manage;
}

bool Scene::ChildrenAreManaged()
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
}

void Scene::RemoveModel(ModelReference reference)
{
	std::vector<Model*>::iterator result = this->m_models.end();
	for (std::vector<Model*>::iterator it = this->m_models.begin(); it != this->m_models.end(); it++)
	{
		if ((*it)->GetReference() == reference)
		{
			result = it;
		}
	}

	if (result != this->m_models.end())
	{
		this->m_models.erase(result);

		if (this->m_manage_children)
		{
			delete *result;
		}
	}
}

Model* Scene::GetModel(ModelReference reference)
{
	for (std::vector<Model*>::iterator it = this->m_models.begin(); it != this->m_models.end(); it++)
	{
		if ((*it)->GetReference() == reference)
		{
			return *it;
		}
	}

	return nullptr;
}

std::vector<Model*> Scene::GetModels()
{
	return this->m_models;
}

void Scene::SetAmbientLight(glm::vec3 light_intensity)
{
	this->m_light_ambient = light_intensity;
}

glm::vec3 Scene::GetAmbientLight()
{
	return this->m_light_ambient;
}

ModelReference Scene::GetNewModelReference()
{
	return this->m_reference_model++;
}

CubemapReference Scene::GetNewCubemapReference()
{
	return this->m_reference_cubemap++;
}

TextureReference Scene::GetNewTextureReference()
{
	return this->m_reference_texture;
}

void Scene::SetSkyboxScene(Scene* scene)
{
	this->m_skybox_scene = scene;
}

Scene* Scene::GetSkyboxScene()
{
	return this->m_skybox_scene;
}

void Scene::SetClearColour(glm::vec4 colour)
{
	this->m_clear_colour = colour;
}

glm::vec4 Scene::GetClearColour()
{
	return this->m_clear_colour;
}

std::vector<Model*> Scene::GetVisibleModels(glm::dvec3 position, RenderMode mode)
{
	if (mode == RenderMode::Normal)
	{
		std::unordered_set<Model*, HashPointer<Model>> visible_models;
		std::unordered_set<VisBox*, HashPointer<VisBox>> enclosed_visboxes;

		for (size_t i = 0; i < this->m_visboxes.size(); i++)
		{
			if (this->m_visboxes.at(i)->PointInBounds(position))
			{
				enclosed_visboxes.insert(this->m_visboxes.at(i));
			}
		}

		if (enclosed_visboxes.size() == 0) //player is outside of level, draw everything (for navigating back to the level if nothing else)
		{
			for (size_t i = 0; i < this->m_models.size(); i++)
			{
				visible_models.insert(this->m_models.at(i));
			}
		}
		else
		{
			for (auto it = enclosed_visboxes.begin(); it != enclosed_visboxes.end(); it++)
			{
				std::unordered_set<Model*, HashPointer<Model>> locally_visible_models = (*it)->GetPotentiallyVisibleModels();
				visible_models.insert(locally_visible_models.begin(), locally_visible_models.end());
			}
		}

		std::vector<Model*> output;
		output.assign(visible_models.begin(), visible_models.end());

		std::sort(output.begin(), output.end(), [position](Model* a, Model* b) mutable { return glm::length(a->GetPosition() - position) < glm::length(b->GetPosition() - position); }); //true means a should be moved forward and drawn earlier

		return output;
	}
	else if (mode == RenderMode::Editor)
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

std::vector<PointLight*> Scene::GetPointLights()
{
	return this->m_pointlights;
}

void Scene::Add(Reflection* reflection)
{
	this->m_reflections.push_back(reflection);
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
}

std::vector<Reflection*> Scene::GetReflections()
{
	return this->m_reflections;
}

void Scene::Add(VisBox* visbox)
{
	this->m_visboxes.push_back(visbox);
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

std::vector<VisBox*> Scene::GetVisBoxes()
{
	return this->m_visboxes;
}

void Scene::RemoveCubemap(CubemapReference reference)
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
			this->m_reflections.erase(result);

			if (this->m_manage_children)
			{
				delete *result;
			}
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
			this->m_pointlights.erase(result);

			if (this->m_manage_children)
			{
				delete *result;
			}
		}
	}
}

std::tuple<Cubemap*, CubemapType> Scene::GetCubemap(CubemapReference reference)
{
	for (std::vector<Reflection*>::iterator it = this->m_reflections.begin(); it != this->m_reflections.end(); it++)
	{
		if ((*it)->GetReference() == reference)
		{
			return { *it, CubemapType::Reflection };
		}
	}

	for (std::vector<PointLight*>::iterator it = this->m_pointlights.begin(); it != this->m_pointlights.end(); it++)
	{
		if ((*it)->GetReference() == reference)
		{
			return { *it, CubemapType::Pointlight };
		}
	}

	return { nullptr, CubemapType::None };
}

std::vector<CubemapReference> Scene::GetCubemaps()
{
	std::vector<CubemapReference> references;
	for (auto it = this->m_reflections.begin(); it != this->m_reflections.end(); it++)
	{
		references.push_back((*it)->GetReference());
	}

	for (auto it = this->m_pointlights.begin(); it != this->m_pointlights.end(); it++)
	{
		references.push_back((*it)->GetReference());
	}

	return references;
}
