#include <wx/wxprec.h>
#include <GL/glew.h>
#include "Scene.h"

Scene::Scene(Camera* active_camera)
{
	this->m_active_camera = active_camera;
}

Scene::~Scene()
{
	for (size_t i = 0; i < this->models.size(); i++)
	{
		delete this->models.at(i);
	}

	for (size_t i = 0; i < this->cameras.size(); i++)
	{
		delete this->cameras.at(i);
	}
}

int Scene::GetModelIndex(Model* model)
{
	for (size_t i = 0; i < this->models.size(); i++)
	{
		if (model == this->models.at(i))
		{
			return i;
		}
	}
	return -1;
}

int Scene::GetCameraIndex(Camera* camera)
{
	for (size_t i = 0; i < this->cameras.size(); i++)
	{
		if (camera == this->cameras.at(i))
		{
			return i;
		}
	}
	return -1;
}

void Scene::AddModel(Model* model)
{
	this->models.push_back(model);

	if (model->vertex_buffer != 0)
	{
		glGenBuffers(1, &model->vertex_buffer);
		glBindBuffer(GL_ARRAY_BUFFER, model->vertex_buffer);

		std::vector<std::vector<GLfloat>> trifan = model->GetTriFans();
		std::vector<GLfloat> trifan_to_load;
		for (size_t i = 0; i < trifan.size(); i++)
		{
			for (size_t j = 0; j < trifan.at(i).size(); j++)
			{
				trifan_to_load.push_back(trifan.at(i).at(j));
			}
		}

		glBufferData(GL_ARRAY_BUFFER, trifan_to_load.size() * sizeof(GLfloat), trifan_to_load.data(), GL_STATIC_DRAW);
	}
}

void Scene::RemoveModel(Model* model)
{
	this->models.erase(this->models.begin() + this->GetModelIndex(model));
}

void Scene::AddCamera(Camera* camera)
{
	this->cameras.push_back(camera);
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

size_t Scene::NumModels()
{
	return this->models.size();
}

size_t Scene::NumCameras()
{
	return this->cameras.size();
}

void Scene::ClearAllModels(bool destroy)
{
	if (destroy)
	{
		for (size_t i = 0; i < this->models.size(); i++)
		{
			delete this->models.at(i);
		}
	}
	
	this->models.clear();
}

void Scene::ClearAllCameras(bool destroy)
{
	if (destroy)
	{
		for (size_t i = 0; i < this->cameras.size(); i++)
		{
			delete this->cameras.at(i);
		}
	}

	this->cameras.clear();
}

void Scene::Render(EngineCanvas* canvas)
{
	
}