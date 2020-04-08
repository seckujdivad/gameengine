#include <wx/wxprec.h>
#include "Scene.h"

Scene::Scene(Camera* active_camera)
{
	this->m_active_camera = active_camera;
	this->m_identifier = "";

	this->InitialiseSkyboxTexture(1, 1);
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

	for (size_t i = 0; i < this->reflections.size(); i++)
	{
		delete this->reflections.at(i);
	}

	if (this->m_skybox_texture != NULL)
	{
		glDeleteTextures(1, &this->m_skybox_texture);
	}
	
	if (this->m_skybox_fbo != NULL)
	{
		glDeleteFramebuffers(1, &this->m_skybox_fbo);
	}

	delete this->m_skybox_scene;
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

int Scene::GetReflectionIndex(Reflection* reflection)
{
	for (size_t i = 0; i < this->reflections.size(); i++)
	{
		if (reflection == this->reflections.at(i))
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
	model->GetShaderProgram()->RegisterTexture("skyboxTexture", this->m_skybox_texture, GL_TEXTURE_CUBE_MAP);
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

void Scene::AddReflection(Reflection* reflection)
{
	this->reflections.push_back(reflection);
}

void Scene::RemoveReflection(Reflection* reflection)
{
	int reflection_index = this->GetReflectionIndex(reflection);
	if (reflection_index == -1)
	{
		throw std::runtime_error("PointLight doesn't exist in this scene");
	}
	else
	{
		this->reflections.erase(this->reflections.begin() + reflection_index);
	}
}

Nameable* Scene::GetByIdentifier(std::string identifier, int type)
{
	size_t size;
	switch (type)
	{
	case 0: size = this->models.size(); break;
	case 1: size = this->cameras.size(); break;
	case 2: size = this->pointlights.size(); break;
	case 3: size = this->reflections.size(); break;
	default: return nullptr;
	}

	Nameable* object;
	for (size_t i = 0; i < size; i++)
	{
		switch (type)
		{
		case 0: object = this->models.at(i); break;
		case 1: object = this->cameras.at(i); break;
		case 2: object = this->pointlights.at(i); break;
		case 3: object = this->reflections.at(i); break;
		default: return nullptr;
		}

		if (object->GetIdentifier() == identifier)
		{
			return object;
		}
	}

	return nullptr;
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

	//draw reflections
	this->DrawReflections(1);

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
			if ((mode == 0) || ((mode == 1) && this->pointlights.at(i)->DynamicNeedsRedrawing(true)))
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

			this->pointlights.at(i)->IncrementFrameCounter(1);
		}
	}
}

void Scene::DrawReflections(int mode)
{
	for (size_t i = 0; i < this->models.size(); i++)
	{
		this->models.at(i)->GenPosMat();
	}

	glCullFace(GL_BACK);
	glClearColor(1.0f, 1.0f, 1.0f, 1.0f);

	for (size_t i = 0; i < this->reflections.size(); i++)
	{
		if ((mode == 0) || ((mode == 1) && this->reflections.at(i)->DynamicNeedsRedrawing(true)))
		{
			if (mode == 1)
			{
				this->reflections.at(i)->CopyStaticToDynamic();
			}

			for (int face = 0; face < 6; face++)
			{
				this->reflections.at(i)->SelectFBO(face);

				if (mode == 0)
				{
					glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
				}

				for (size_t j = 0; j < this->models.size(); j++)
				{
					bool draw_model;
					if (mode == 0)
					{
						draw_model = this->reflections.at(i)->ModelIsStatic(this->models.at(j)->GetIdentifier());
					}
					else
					{
						draw_model = this->reflections.at(i)->ModelIsDynamic(this->models.at(j)->GetIdentifier());
					}

					if (draw_model)
					{
						this->models.at(j)->GetShaderProgram()->Select();

						glUniform3fv(this->models.at(j)->GetShaderProgram()->GetUniform("light_ambient"), 1, glm::value_ptr(this->m_light_ambient));
						
						this->models.at(j)->SetUniforms();
						this->reflections.at(i)->SetGenerateUniforms(this->models.at(j)->GetShaderProgram(), face); //overwrite some uniforms - this call MUST come after Model::SetUniforms as it calls Reflection::SetUniforms (which sets different uniform values to Reflection::SetGenerateUniforms)

						for (size_t k = 0; k < this->pointlights.size(); k++)
						{
							this->pointlights.at(k)->SetUniforms(this->models.at(j)->GetShaderProgram());
						}

						this->reflections.at(i)->InitialiseViewport();
						this->models.at(j)->BindVAO();
						this->models.at(j)->DrawVBOs();
					}
				}
			}

			if (mode == 0)
			{
				this->reflections.at(i)->CopyDynamicToStatic();
			}
		}

		this->reflections.at(i)->IncrementFrameCounter(1);
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

		for (size_t j = 0; j < this->reflections.size(); j++)
		{
			this->reflections.at(j)->RegisterUniforms(this->models.at(i)->GetShaderProgram());
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

void Scene::InitialiseSkyboxTexture(unsigned int texture_width, unsigned int texture_height)
{
	this->m_skybox_texture_dimensions[0] = texture_width;
	this->m_skybox_texture_dimensions[1] = texture_height;

	if (this->m_skybox_texture != NULL)
	{
		glDeleteTextures(1, &this->m_skybox_texture);
	}

	if (this->m_skybox_fbo != NULL)
	{
		glDeleteFramebuffers(1, &this->m_skybox_fbo);
	}

	glGenTextures(1, &this->m_skybox_texture);
	glBindTexture(GL_TEXTURE_CUBE_MAP, this->m_skybox_texture);
	for (int i = 0; i < 6; i++)
	{
		glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, GL_RGB, this->m_skybox_texture_dimensions[0], this->m_skybox_texture_dimensions[1], 0, GL_RGB, GL_UNSIGNED_BYTE, NULL);
	}
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	glGenFramebuffers(1, &this->m_skybox_fbo);
	glBindFramebuffer(GL_FRAMEBUFFER, this->m_skybox_fbo);
	glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, this->m_skybox_texture, 0);

	GLenum framebuffer_status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
	if (framebuffer_status != GL_FRAMEBUFFER_COMPLETE)
	{
		throw std::runtime_error("Framebuffer error, status " + std::to_string(framebuffer_status));
	}

	glBindFramebuffer(GL_FRAMEBUFFER, 0);

	for (size_t i = 0; i < this->models.size(); i++)
	{
		this->models.at(i)->GetShaderProgram()->UpdateTexture("skyboxTexture", this->m_skybox_texture);
	}
}

void Scene::SetSkyboxScene(Scene* scene)
{
	this->m_skybox_scene = scene;

	this->m_skybox_scene->PushUniforms();
	this->m_skybox_scene->DrawShadows(0);
	this->m_skybox_scene->DrawReflections(0);
}

void Scene::DrawSkyboxScene()
{
	if (this->m_skybox_scene != nullptr)
	{
		glBindFramebuffer(GL_FRAMEBUFFER, this->m_skybox_fbo);
		glViewport(0, 0, this->m_skybox_texture_dimensions[0], this->m_skybox_texture_dimensions[1]);

		Camera* scene_camera = this->m_skybox_scene->GetActiveCamera();

		int rotation_change[3] = { 0, 0, 0 };

		//render all six sides of the skybox texture
		for (int i = 0; i < 6; i++)
		{
			glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, this->m_skybox_texture, 0);

			if (i == 0) //x+
			{
				rotation_change[0] = 90;
				rotation_change[1] = 0;
				rotation_change[2] = -90;
			}
			else if (i == 1) //x-
			{
				rotation_change[0] = 90;
				rotation_change[1] = 0;
				rotation_change[2] = 90;
			}
			else if(i == 2) //y+
			{
				rotation_change[0] = 90;
				rotation_change[1] = 0;
				rotation_change[2] = 0;
			}
			else if(i == 3) //y-
			{
				rotation_change[0] = 90;
				rotation_change[1] = 0;
				rotation_change[2] = 180;
			}
			else if(i == 4) //z+
			{
				rotation_change[0] = 180;
				rotation_change[1] = 0;
				rotation_change[2] = 0;
			}
			else if(i == 5) //z-
			{
				rotation_change[0] = 0;
				rotation_change[1] = 0;
				rotation_change[2] = 0;
			}

			for (int j = 0; j < 3; j++)
			{
				scene_camera->SetRotation(j, scene_camera->GetRotation(j) + rotation_change[j]);
			}

			this->m_skybox_scene->Render(this->m_skybox_fbo);

			for (int j = 0; j < 3; j++)
			{
				scene_camera->SetRotation(j, scene_camera->GetRotation(j) - rotation_change[j]);
			}
		}

		glFlush();
		glBindFramebuffer(GL_FRAMEBUFFER, 0);
	}
}
