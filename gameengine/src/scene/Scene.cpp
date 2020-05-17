#include <wx/wxprec.h>
#include "Scene.h"

Scene::Scene() : Nameable()
{
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

	for (size_t i = 0; i < this->visboxes.size(); i++)
	{
		delete this->visboxes.at(i);
	}

	for (size_t i = 0; i < this->m_shader_programs.size(); i++)
	{
		delete this->m_shader_programs.at(i);
	}

	if (this->m_skybox_texture != NULL)
	{
		glDeleteTextures(1, &this->m_skybox_texture);
	}
	
	if (this->m_skybox_fbo != NULL)
	{
		glDeleteFramebuffers(1, &this->m_skybox_fbo);
	}

	delete this->m_approximation;
	delete this->m_skybox_scene;
}

int Scene::GetModelIndex(Model* model)
{
	for (int i = 0; i < (int)this->models.size(); i++)
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
	for (int i = 0; i < (int)this->cameras.size(); i++)
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
	for (int i = 0; i < (int)this->pointlights.size(); i++)
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
	for (int i = 0; i < (int)this->reflections.size(); i++)
	{
		if (reflection == this->reflections.at(i))
		{
			return i;
		}
	}
	return -1;
}

int Scene::GetVisBoxIndex(VisBox* visbox)
{
	for (int i = 0; i < (int)this->visboxes.size(); i++)
	{
		if (visbox == this->visboxes.at(i))
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
	model->GetShaderProgram()->RegisterTexture("render_output_colour", this->m_output_colour, GL_TEXTURE_2D);
	model->GetShaderProgram()->RegisterTexture("render_output_depth", this->m_output_depth, GL_TEXTURE_2D);
	model->GetShaderProgram()->RegisterUniform("render_output_valid");
	model->GetShaderProgram()->RegisterUniform("render_output_x");
	model->GetShaderProgram()->RegisterUniform("render_output_y");
	
	for (int i = 0; i < (int)this->m_output_data.size(); i++)
	{
		model->GetShaderProgram()->RegisterTexture("render_output_data[" + std::to_string(i) + "]", this->m_output_data.at(i), GL_TEXTURE_2D);
	}

	for (int i = (int)this->m_output_data.size(); i < ENGINECANVAS_NUM_DATA_TEX; i++)
	{
		model->GetShaderProgram()->RegisterTexture("render_output_data[" + std::to_string(i) + "]", NULL, GL_TEXTURE_2D);
	}
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
	else
	{
		this->cameras.erase(this->cameras.begin() + camera_index);
	}
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
		throw std::runtime_error("Reflection doesn't exist in this scene");
	}
	else
	{
		this->reflections.erase(this->reflections.begin() + reflection_index);
	}
}

void Scene::AddVisBox(VisBox* visbox)
{
	this->visboxes.push_back(visbox);
}

void Scene::RemoveVisBox(VisBox* visbox)
{
	int visbox_index = this->GetVisBoxIndex(visbox);
	if (visbox_index == -1)
	{
		throw std::runtime_error("VisBox doesn't exist in this scene");
	}
	else
	{
		this->visboxes.erase(this->visboxes.begin() + visbox_index);
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
	case 4: size = this->visboxes.size(); break;
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
		case 4: object = this->visboxes.at(i); break;
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

void Scene::Render(GLuint framebuffer, Camera* camera)
{
	//get viewport dimensions so it can be reset
	GLint viewport_dimensions[4];
	glGetIntegerv(GL_VIEWPORT, viewport_dimensions);
	
	//draw shadows
	this->DrawShadows(1);

	//draw reflections
	this->DrawReflections(1);

	//prepare for camera draw
	glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);

	glCullFace(GL_BACK);
	glViewport(viewport_dimensions[0], viewport_dimensions[1], viewport_dimensions[2], viewport_dimensions[3]);
	glClearColor(this->m_clear_colour.r, this->m_clear_colour.g, this->m_clear_colour.b, this->m_clear_colour.a);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	camera->SetViewportDimensions(viewport_dimensions[2] - viewport_dimensions[0], viewport_dimensions[3] - viewport_dimensions[1]);

	//draw scene
	std::vector<Model*> models_to_draw = this->GetVisibleModels(glm::vec3(camera->GetPosition(0),
		camera->GetPosition(1),
		camera->GetPosition(2)));

	ShaderProgram* model_shader_program = nullptr;

	for (auto it = models_to_draw.begin(); it != models_to_draw.end(); it++)
	{
		Model* model = *it;

		if (model_shader_program != model->GetShaderProgram())
		{
			model_shader_program = model->GetShaderProgram();
			model_shader_program->Select();
		}

		glUniform1i(model_shader_program->GetUniform("render_output_x"), viewport_dimensions[2] - viewport_dimensions[0]);
		glUniform1i(model_shader_program->GetUniform("render_output_y"), viewport_dimensions[3] - viewport_dimensions[1]);

		this->m_approximation->SetUniforms(model_shader_program);

		glUniform3fv(model_shader_program->GetUniform("light_ambient"), 1, glm::value_ptr(this->m_light_ambient));
		camera->SetUniforms(model_shader_program);
		model->SetUniforms();

		for (size_t j = 0; j < this->pointlights.size(); j++)
		{
			this->pointlights.at(j)->SetUniforms(model_shader_program);
		}

		if (this->m_output_colour == NULL)
		{
			glUniform1i(model_shader_program->GetUniform("render_output_valid"), false);
		}
		else
		{
			glUniform1i(model_shader_program->GetUniform("render_output_valid"), true);
		}

		model->BindVAO();
		model->DrawVBOs();
	}
}

void Scene::DrawShadows(int mode) //0: static, 1: dynamic
{
	glCullFace(GL_FRONT);

	bool draw_model;

	for (size_t i = 0; i < this->pointlights.size(); i++)
	{
		if (this->pointlights.at(i)->ShadowsEnabled())
		{
			if ((mode == 0) || ((mode == 1) && this->pointlights.at(i)->DynamicNeedsRedrawing(true)))
			{
				std::vector<Model*> models_to_draw = this->GetVisibleModels(glm::vec3(this->pointlights.at(i)->GetPosition(0),
					this->pointlights.at(i)->GetPosition(1),
					this->pointlights.at(i)->GetPosition(2)));

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

				for (auto it = models_to_draw.begin(); it != models_to_draw.end(); it++)
				{
					Model* model = *it;

					if (mode == 0)
					{
						draw_model = this->pointlights.at(i)->ModelIsStatic(model->GetIdentifier());
					}
					else
					{
						draw_model = this->pointlights.at(i)->ModelIsDynamic(model->GetIdentifier());
					}

					if (draw_model)
					{
						model->GetShadowShaderProgram()->Select();
						model->BindVAO();
						model->SetShadowUniforms();
						this->pointlights.at(i)->SetShadowUniforms(model->GetShadowShaderProgram());
						model->DrawVBOs();
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
	glCullFace(GL_BACK);
	glClearColor(this->m_clear_colour.r, this->m_clear_colour.g, this->m_clear_colour.b, this->m_clear_colour.a);

	for (size_t i = 0; i < this->reflections.size(); i++)
	{
		Reflection* reflection = this->reflections.at(i);

		if ((mode == 0) || ((mode == 1) && reflection->DynamicNeedsRedrawing(true)))
		{
			if (mode == 1)
			{
				reflection->CopyStaticToDynamic();
			}

			std::vector<Model*> models = this->GetVisibleModels(reflection->GetPositionVec());

			for (int face = 0; face < 6; face++)
			{
				reflection->SelectFBO(face);

				if (mode == 0)
				{
					glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
				}

				for (auto it = models.begin(); it != models.end(); it++)
				{
					Model* model = *it;

					bool draw_model;
					if (mode == 0)
					{
						draw_model = reflection->ModelIsStatic(model->GetIdentifier());
					}
					else
					{
						draw_model = reflection->ModelIsDynamic(model->GetIdentifier());
					}

					if (draw_model)
					{
						model->GetShaderProgram()->Select();

						glUniform3fv(model->GetShaderProgram()->GetUniform("light_ambient"), 1, glm::value_ptr(this->m_light_ambient));
						
						model->SetUniforms();
						reflection->SetGenerateUniforms(model->GetShaderProgram(), face); //overwrite some uniforms - this call MUST come after Model::SetUniforms as it calls Reflection::SetUniforms (which sets different uniform values to Reflection::SetGenerateUniforms)

						for (size_t k = 0; k < this->pointlights.size(); k++)
						{
							this->pointlights.at(k)->SetUniforms(model->GetShaderProgram());
						}

						glUniform1i(model->GetShaderProgram()->GetUniform("mat_ssr_enabled"), GL_FALSE);

						reflection->InitialiseViewport();
						model->BindVAO();
						model->DrawVBOs();
					}
				}
			}

			if (mode == 0)
			{
				reflection->CopyDynamicToStatic();
			}
		}

		reflection->IncrementFrameCounter(1);
	}
}

void Scene::PushUniforms()
{
	for (size_t i = 0; i < this->models.size(); i++)
	{
		this->models.at(i)->RegisterUniforms();
		this->models.at(i)->RegisterShadowUniforms();

		this->models.at(i)->GetShaderProgram()->RegisterUniform("light_ambient");

		this->m_approximation->RegisterUniforms(this->models.at(i)->GetShaderProgram());

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
		std::string status = "unknown status";

		if (framebuffer_status == GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT)
		{
			status = "incomplete attachment";
		}
		else if(framebuffer_status == GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT)
		{
			status = "missing attachment";
		}
		else if(framebuffer_status == GL_FRAMEBUFFER_UNSUPPORTED)
		{
			status = "unsupported";
		}

		throw std::runtime_error("Framebuffer error, status " + std::to_string(framebuffer_status) + " (" + status + ")");
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

		Camera* scene_camera = this->m_skybox_scene->cameras.at(0);

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

			this->m_skybox_scene->Render(this->m_skybox_fbo, scene_camera);

			for (int j = 0; j < 3; j++)
			{
				scene_camera->SetRotation(j, scene_camera->GetRotation(j) - rotation_change[j]);
			}
		}

		glFlush();
		glBindFramebuffer(GL_FRAMEBUFFER, 0);
	}
}

void Scene::SetClearColour(glm::vec4 colour)
{
	this->m_clear_colour = colour;
}

glm::vec4 Scene::GetClearColour()
{
	return this->m_clear_colour;
}

std::vector<Model*> Scene::GetVisibleModels(glm::vec3 position)
{
	std::unordered_set<Model*, HashPointer<Model>> visible_models;
	std::unordered_set<VisBox*, HashPointer<VisBox>> enclosed_visboxes;

	for (size_t i = 0; i < this->visboxes.size(); i++)
	{
		if (this->visboxes.at(i)->PointInBounds(position))
		{
			enclosed_visboxes.insert(this->visboxes.at(i));
		}
	}

	if (enclosed_visboxes.size() == 0) //player is outside of level, draw everything (for navigating back to the level if nothing else)
	{
		for (size_t i = 0; i < this->models.size(); i++)
		{
			visible_models.insert(this->models.at(i));
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

	std::sort(output.begin(), output.end(), [position](Model* a, Model* b) mutable { return glm::length(a->GetPositionVec() - position) < glm::length(b->GetPositionVec() - position); }); //true means a should be moved forward and drawn earlier

	return output;
}

void Scene::SetReceivedOutputTextures(GLuint colour, GLuint depth, std::vector<GLuint> data)
{
	this->m_output_colour = colour;
	this->m_output_depth = depth;
	this->m_output_data = data;

	for (int i = 0; i < (int)this->models.size(); i++)
	{
		this->models.at(i)->GetShaderProgram()->UpdateTexture("render_output_colour", this->m_output_colour);
		this->models.at(i)->GetShaderProgram()->UpdateTexture("render_output_depth", this->m_output_depth);

		for (int j = 0; j < (int)this->m_output_data.size(); j++)
		{
			this->models.at(i)->GetShaderProgram()->UpdateTexture("render_output_data[" + std::to_string(j) + "]", this->m_output_data.at(j));
		}
	}
}

ShaderProgram* Scene::GetShaderProgram(ShaderDescription description)
{
	for (int i = 0; i < (int)this->m_shader_descriptions.size(); i++)
	{
		if ((description.shaders == this->m_shader_descriptions.at(i).shaders) && (description.preprocessor_defines == this->m_shader_descriptions.at(i).preprocessor_defines))
		{
			return this->m_shader_programs.at(i);
		}
	}

	ShaderProgram* new_program = new ShaderProgram(description.shaders, description.preprocessor_defines);
	this->m_shader_programs.push_back(new_program);
	return new_program;
}

void Scene::SetApproximation(SceneApproximation* approximation)
{
	if (this->m_approximation != nullptr)
	{
		delete this->m_approximation;
	}

	this->m_approximation = approximation;
}
