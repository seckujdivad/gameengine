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

	for (size_t i = 0; i < this->pointlights.size(); i++)
	{
		delete this->pointlights.at(i);
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

int Scene::GetPointLightIndex(PointLight* pointlight)
{
	for (size_t i = 0; i < this->pointlights.size(); i++)
	{
		if (pointlight == this->pointlights.at(i))
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
	int model_index = this->GetModelIndex(model);
	if (model_index == -1)
	{
		throw std::runtime_error("Model doesn't exist in this scene");
	}
	else
	{
		this->models.erase(this->models.begin() + model_index);
	}
}

void Scene::AddCamera(Camera* camera)
{
	this->cameras.push_back(camera);
}

void Scene::RemoveCamera(Camera* camera)
{
	int camera_index = this->GetCameraIndex(camera);
	if (camera_index == -1)
	{
		throw std::runtime_error("Camera doesn't exist in this scene");
	}
	else if (camera == this->m_active_camera)
	{
		throw std::runtime_error("Can't remove the active camera");
	}
	else
	{
		this->cameras.erase(this->cameras.begin() + camera_index);
	}
}

void Scene::SetActiveCamera(Camera* camera)
{
	this->m_active_camera = camera;
}

Camera* Scene::GetActiveCamera()
{
	return this->m_active_camera;
}

void Scene::AddPointLight(PointLight* pointlight)
{
	this->pointlights.push_back(pointlight);
}

void Scene::RemovePointLight(PointLight* pointlight)
{
	int pointlight_index = this->GetPointLightIndex(pointlight);
	if (pointlight_index == -1)
	{
		throw std::runtime_error("PointLight doesn't exist in this scene");
	}
	else
	{
		this->pointlights.erase(this->pointlights.begin() + pointlight_index);
	}
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

void Scene::Render(GLuint framebuffer)
{
	//get viewport dimensions so it can be reset
	GLint viewport_dimensions[4];
	glGetIntegerv(GL_VIEWPORT, viewport_dimensions);

	glClearColor(1.0f, 1.0f, 1.0f, 1.0f);

	//generate values
	for (size_t i = 0; i < this->models.size(); i++)
	{
		this->models.at(i)->GenPosMat();
	}
	
	//draw shadows
	this->DrawShadows(1);

	//prepare for camera draw
	glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);
	glCullFace(GL_BACK);
	glViewport(viewport_dimensions[0], viewport_dimensions[1], viewport_dimensions[2], viewport_dimensions[3]);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	this->m_active_camera->GenPerspMat((float)(viewport_dimensions[2] - viewport_dimensions[0]), (float)(viewport_dimensions[3] - viewport_dimensions[1]));
	this->m_active_camera->GenViewMat();

	//draw scene
	for (size_t i = 0; i < this->models.size(); i++)
	{
		this->models.at(i)->GetShaderProgram()->Select();
		this->models.at(i)->BindVAO();

		glUniform3fv(this->models.at(i)->GetShaderProgram()->GetUniform("light_ambient"), 1, glm::value_ptr(this->m_light_ambient));
		this->m_active_camera->SetUniforms(this->models.at(i)->GetShaderProgram());
		this->models.at(i)->SetUniforms();

		for (size_t j = 0; j < this->pointlights.size(); j++)
		{
			this->pointlights.at(j)->SetUniforms(this->models.at(i)->GetShaderProgram());
		}

		this->models.at(i)->DrawVBOs();
	}
}

void Scene::DrawShadows(int mode) //0: static, 1: dynamic
{
	for (size_t i = 0; i < this->models.size(); i++)
	{
		this->models.at(i)->GenPosMat();
	}

	glCullFace(GL_FRONT);

	bool draw_model;

	for (size_t i = 0; i < this->pointlights.size(); i++)
	{
		if (this->pointlights.at(i)->ShadowsEnabled())
		{
			this->pointlights.at(i)->InitialiseViewport();

			if (mode == 0)
			{
				this->pointlights.at(i)->SelectFBO();
				glClear(GL_DEPTH_BUFFER_BIT);
			}
			else
			{
				this->pointlights.at(i)->CopyStaticToDynamic();
				this->pointlights.at(i)->SelectFBO();
			}

			for (size_t j = 0; j < this->models.size(); j++)
			{
				if (mode == 0)
				{
					draw_model = this->pointlights.at(i)->ModelIsStatic(this->models.at(j)->GetIdentifier());
				}
				else
				{
					draw_model = this->pointlights.at(i)->ModelIsDynamic(this->models.at(j)->GetIdentifier());
				}
				

				if (draw_model)
				{
					this->models.at(j)->GetShadowShaderProgram()->Select();
					this->models.at(j)->BindVAO();
					this->models.at(j)->SetShadowUniforms();
					this->pointlights.at(i)->SetShadowUniforms(this->models.at(j)->GetShadowShaderProgram());
					this->models.at(j)->DrawVBOs();
				}
			}

			if (mode == 0)
			{
				this->pointlights.at(i)->CopyDynamicToStatic();
			}
		}
	}
}

void Scene::PushUniforms()
{
	for (size_t i = 0; i < this->models.size(); i++)
	{
		this->models.at(i)->RegisterUniforms();
		this->models.at(i)->RegisterShadowUniforms();

		this->models.at(i)->GetShaderProgram()->RegisterUniform("light_ambient");

		for (size_t j = 0; j < this->cameras.size(); j++)
		{
			this->cameras.at(j)->RegisterUniforms(this->models.at(i)->GetShaderProgram());
		}

		for (size_t j = 0; j < this->pointlights.size(); j++)
		{
			this->pointlights.at(j)->RegisterUniforms(this->models.at(i)->GetShaderProgram());
			this->pointlights.at(j)->RegisterShadowUniforms(this->models.at(i)->GetShadowShaderProgram());
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

void Scene::SetAmbientLight(glm::vec3 light_intensity)
{
	this->m_light_ambient = light_intensity;
}