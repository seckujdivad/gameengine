#include "Scene.h"

int Scene::GetModelIndex(Model* model)
{
	for (size_t i = 0; i < this->m_models.size(); i++)
	{
		if (model == this->m_models.at(i))
		{
			return i;
		}
	}
	return -1;
}

int Scene::GetCameraIndex(Camera* camera)
{
	for (size_t i = 0; i < this->m_cameras.size(); i++)
	{
		if (camera == this->m_cameras.at(i))
		{
			return i;
		}
	}
	return -1;
}

Scene::Scene(Camera* active_camera)
{
	this->m_active_camera = active_camera;
}

Scene::~Scene()
{
}

void Scene::AddModel(Model* model)
{
	this->m_models.push_back(model);
}

void Scene::RemoveModel(Model* model)
{
	this->m_models.erase(this->m_models.begin() + this->GetModelIndex(model));
}

void Scene::AddCamera(Camera* camera)
{
	this->m_cameras.push_back(camera);
}

void Scene::RemoveCamera(Camera* camera)
{
}

void Scene::SetActiveCamera(Camera* camera)
{
	this->m_active_camera = camera;
}

Camera* Scene::GetActiveCamera()
{
	return this->m_active_camera;
}

void Scene::Render(EngineCanvas* canvas)
{

}