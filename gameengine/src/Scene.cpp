#include <wx/wxprec.h>
#include "Scene.h"

Scene::Scene(Camera* active_camera)
{
	this->m_active_camera = active_camera;
	this->m_identifier = "";
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

	model->GenVertexBuffer(GL_TRIANGLES);
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
	glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	this->m_active_camera->GenPerspMat((float)canvas->GetSize().x, (float)canvas->GetSize().y);
	this->m_active_camera->GenViewMat();
	
	for (size_t i = 0; i < this->models.size(); i++)
	{
		this->models.at(i)->GenPosMat();

		this->models.at(i)->GetShaderProgram()->Select();
		this->models.at(i)->BindVAO();

		this->m_active_camera->SetUniforms(this->models.at(i)->GetShaderProgram());
		this->models.at(i)->SetUniforms();

		this->models.at(i)->DrawVBOs();
	}
}

void Scene::PushUniforms()
{
	for (size_t i = 0; i < this->models.size(); i++)
	{
		this->models.at(i)->RegisterUniforms();

		for (size_t j = 0; j < this->cameras.size(); j++)
		{
			this->cameras.at(j)->RegisterUniforms(this->models.at(i)->GetShaderProgram());
		}
	}
}

void Scene::SetIdentifier(std::string identifier)
{
	this->m_identifier = identifier;
}

std::string Scene::GetIdentifier()
{
	return this->m_identifier;
}