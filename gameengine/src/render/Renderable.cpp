#include "Renderable.h"

void Renderable::RenderScene(std::vector<Model*> models)
{
	Scene* scene = this->m_engine->GetScene();
	if (scene != nullptr)
	{
		bool dealloc_models = false;
		//resolve models
		if ((models.size() == 1) && (models.at(0) == nullptr)) //default state
		{
			if ((this->GetRenderMode() == RenderMode::Normal) || (this->GetRenderMode() == RenderMode::Shadow) || (this->GetRenderMode() == RenderMode::Wireframe))
			{
				models = scene->GetVisibleModels(this->m_camera->GetPosition(), this->m_rendermode);
			}
			else if (this->GetRenderMode() == RenderMode::Postprocess)
			{
				models.clear();

				ModelGeometry geom;
				geom.vertices = {
					glm::dvec3(0.0, 0.0, 0.0),
					glm::dvec3(0.0, 1.0, 0.0),
					glm::dvec3(1.0, 1.0, 0.0),
					glm::dvec3(1.0, 0.0, 0.0)
				};

				geom.faces = {
					Face(),
					Face()
				};

				geom.faces.at(0).vertices = { 0, 1, 2 };
				geom.faces.at(0).uv = {
					glm::dvec2(0.0, 0.0),
					glm::dvec2(0.0, 1.0),
					glm::dvec2(1.0, 1.0)
				};
				geom.faces.at(0).normal = glm::dvec3(0.0, 0.0, -1.0);

				geom.faces.at(1).vertices = { 0, 3, 2 };
				geom.faces.at(1).uv = {
					glm::dvec2(0.0, 0.0),
					glm::dvec2(0.1, 0.0),
					glm::dvec2(1.0, 1.0)
				};
				geom.faces.at(1).normal = glm::dvec3(0.0, 0.0, -1.0);

				models.push_back(new Model(-1, geom));

				dealloc_models = true;
			}
		}

		//set preprocessor defines and recompile if any have changed
		bool recompile_required = false;

		if (this->GetRenderMode() == RenderMode::Normal)
		{
			//point lights
			recompile_required = this->SetShaderDefine("POINT_LIGHT_NUM", std::to_string(this->GetEngine()->GetScene()->GetPointLights().size())) ? true : recompile_required;

			//data textures
			recompile_required = this->SetShaderDefine("DATA_TEX_NUM", std::to_string(ENGINECANVAS_NUM_DATA_TEX)) ? true : recompile_required;

			//OBB approximations
			recompile_required = this->SetShaderDefine("APPROXIMATION_OBB_NUM", std::to_string(this->GetEngine()->GetScene()->GetOBBApproximations().size())) ? true : recompile_required;

			//reflections
			recompile_required = this->SetShaderDefine("REFLECTION_NUM", std::to_string(this->GetEngine()->GetScene()->GetReflections().size())) ? true : recompile_required;
		}

		if (recompile_required)
		{
			this->RecompileShader();
		}

		this->m_shader_program->Select();

		//load "constant" uniforms (uniforms constant between models like camera data) into program
		// cubemap uniforms
		{
			std::vector<glm::mat4> transforms;
			glm::vec3 translate = glm::vec3(0.0f);
			transforms.push_back(glm::lookAt(translate, translate + glm::vec3(1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
			transforms.push_back(glm::lookAt(translate, translate + glm::vec3(-1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
			transforms.push_back(glm::lookAt(translate, translate + glm::vec3(0.0f, 1.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f)));
			transforms.push_back(glm::lookAt(translate, translate + glm::vec3(0.0f, -1.0f, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f)));
			transforms.push_back(glm::lookAt(translate, translate + glm::vec3(0.0f, 0.0f, 1.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
			transforms.push_back(glm::lookAt(translate, translate + glm::vec3(0.0f, 0.0f, -1.0f), glm::vec3(0.0f, -1.0f, 0.0f)));

			for (int i = 0; i < (int)transforms.size(); i++)
			{
				this->SetShaderUniform("cubemap_transform[" + std::to_string(i) + "]", transforms.at(i));
			}

			this->SetShaderUniform("is_cubemap", this->m_fbo_target_type == GL_TEXTURE_CUBE_MAP);
		}

		//specialised uniforms
		if (this->GetRenderMode() == RenderMode::Postprocess)
		{
			LoadedTexture texture;
			texture.id = this->m_rendermode_data_postprocess.texture.colour;
			texture.type = GL_TEXTURE_2D;
			texture.uniform_name = "render_output";

			this->m_shader_program->SetTexture(0, texture);
		}
		else if (this->GetRenderMode() == RenderMode::Shadow)
		{
			glm::vec3 translate = this->GetCamera()->GetPosition();
			glm::mat4 projection = this->GetCamera()->GetPerspectiveMatrix();

			std::vector<glm::mat4> transforms;
			transforms.push_back(projection * glm::lookAt(translate, translate + glm::vec3(1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
			transforms.push_back(projection * glm::lookAt(translate, translate + glm::vec3(-1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
			transforms.push_back(projection * glm::lookAt(translate, translate + glm::vec3(0.0f, 1.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f)));
			transforms.push_back(projection * glm::lookAt(translate, translate + glm::vec3(0.0f, -1.0f, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f)));
			transforms.push_back(projection * glm::lookAt(translate, translate + glm::vec3(0.0f, 0.0f, 1.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
			transforms.push_back(projection * glm::lookAt(translate, translate + glm::vec3(0.0f, 0.0f, -1.0f), glm::vec3(0.0f, -1.0f, 0.0f)));

			for (int i = 0; i < (int)transforms.size(); i++)
			{
				this->SetShaderUniform("cubemap_transform[" + std::to_string(i) + "]", transforms.at(i));
			}

			this->SetShaderUniform("is_cubemap", true);

			this->SetShaderUniform("light_position", this->GetCamera()->GetPosition());
			this->SetShaderUniform("light_far_plane", std::get<1>(this->GetCamera()->GetClips()));
			this->SetShaderUniform("light_near_plane", std::get<0>(this->GetCamera()->GetClips()));
		}
		else //normal or wireframe
		{
			// camera
			this->SetShaderUniform("cam_translate", glm::vec4(0.0 - this->GetCamera()->GetPosition(), 0.0f));
			this->SetShaderUniform("cam_rotate", this->GetCamera()->GetRotationMatrixInverse());
			this->SetShaderUniform("cam_persp", this->GetCamera()->GetPerspectiveMatrix());
			this->SetShaderUniform("cam_clip_near", std::get<0>(this->GetCamera()->GetClips()));
			this->SetShaderUniform("cam_clip_far", std::get<1>(this->GetCamera()->GetClips()));
			this->SetShaderUniform("cam_transform", this->GetCamera()->GetCombinedMatrix());
			this->SetShaderUniform("cam_transform_inverse", glm::inverse(this->GetCamera()->GetCombinedMatrix()));

			//shading mode
			this->SetShaderUniform("shade_mode", this->GetRenderMode() == RenderMode::Wireframe ? 1 : 0);

			// ambient light
			this->SetShaderUniform("light_ambient", scene->GetAmbientLight());

			//point lights
			std::vector<PointLight*> point_lights = this->GetEngine()->GetScene()->GetPointLights();
			for (int i = 0; i < (int)point_lights.size(); i++)
			{
				PointLight* point_light = point_lights.at(i);
				std::string root_name = "light_points[" + std::to_string(i) + "].";

				this->AddShaderUniformName(root_name + "position");
				this->SetShaderUniform(root_name + "position", point_light->GetPosition());

				this->AddShaderUniformName(root_name + "intensity");
				this->SetShaderUniform(root_name + "intensity", point_light->GetIntensity());

				this->AddShaderUniformName(root_name + "shadows_enabled");
				this->SetShaderUniform(root_name + "shadows_enabled", point_light->GetShadowsEnabled());

				this->AddShaderUniformName(root_name + "shadow_far_plane");
				this->SetShaderUniform(root_name + "shadow_far_plane", std::get<1>(point_light->GetClips()));

				this->AddShaderUniformName(root_name + "shadow_bias");
				this->SetShaderUniform(root_name + "shadow_bias", point_light->GetShadowBias());

				this->AddShaderUniformName(root_name + "shadow_cubemap");
				RenderTextureGroup texture = this->GetEngine()->GetRenderTexture(point_light->GetReference());
				LoadedTexture loaded_texture;
				loaded_texture.id = texture.colour;
				loaded_texture.type = texture.type;
				loaded_texture.uniform_name = root_name + "shadow_cubemap";

				this->m_shader_program->SetTexture(-1, loaded_texture);
			}

			//scene approximation
			std::vector<OrientedBoundingBox> scene_approximations = this->GetEngine()->GetScene()->GetOBBApproximations();
			for (int i = 0; i < (int)scene_approximations.size(); i++)
			{
				OrientedBoundingBox obb = scene_approximations.at(i);
				std::string prefix = "scene_approximations[" + std::to_string(i) + "].";

				this->AddShaderUniformName(prefix + "position");
				this->SetShaderUniform(prefix + "position", obb.GetPosition());
				
				this->AddShaderUniformName(prefix + "dimensions");
				this->SetShaderUniform(prefix + "dimensions", obb.GetDimensionsVec());

				this->AddShaderUniformName(prefix + "rotation");
				this->SetShaderUniform(prefix + "rotation", obb.GetRotationMatrix());

				this->AddShaderUniformName(prefix + "rotation_inverse");
				this->SetShaderUniform(prefix + "rotation_inverse", obb.GetInverseRotationMatrix());
			}

			//skybox cubemap
			{
				RenderTextureGroup render_texture = this->GetEngine()->GetRenderTexture(this->GetEngine()->GetScene()->GetSkyboxTextureReference());
				LoadedTexture loaded_texture;
				loaded_texture.id = render_texture.colour;
				loaded_texture.type = render_texture.type;
				loaded_texture.uniform_name = "skyboxTexture";
				this->m_shader_program->SetTexture(-1, loaded_texture);
			}

			//previous render result
			{
				this->SetShaderUniform("render_output_valid", this->FramebufferContainsRenderOutput());
				this->SetShaderUniform("render_output_x", std::get<0>(this->GetOutputSize()));
				this->SetShaderUniform("render_output_y", std::get<1>(this->GetOutputSize()));

				//load textures from the previous frame (if in normal rendering mode)
				if (this->GetRenderMode() == RenderMode::Normal)
				{
					LoadedTexture texture;
					texture.type = this->m_rendermode_data_normal.previous_frame.type;
					
					texture.id = this->m_rendermode_data_normal.previous_frame.colour;
					texture.uniform_name = "render_output_colour";
					this->m_shader_program->SetTexture(-1, texture);

					texture.id = this->m_rendermode_data_normal.previous_frame.depth;
					texture.uniform_name = "render_output_depth";
					this->m_shader_program->SetTexture(-1, texture);

					for (int i = 0; i < (int)this->m_rendermode_data_normal.previous_frame.data.size(); i++)
					{
						texture.id = this->m_rendermode_data_normal.previous_frame.data.at(i);
						texture.uniform_name = "render_output_data[" + std::to_string(i) + "]";
						this->m_shader_program->SetTexture(-1, texture);
					}
				}
			}
		}

		//prepare viewport
		glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);
		glViewport(0, 0, std::get<0>(this->GetOutputSize()), std::get<1>(this->GetOutputSize()));
		glClearColor(
			scene->GetClearColour().r,
			scene->GetClearColour().g,
			scene->GetClearColour().b,
			scene->GetClearColour().a
		);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		switch (this->GetRenderMode())
		{
		case RenderMode::Normal:
			glEnable(GL_CULL_FACE);
			glCullFace(GL_BACK);
			break;
		case RenderMode::Postprocess:
			glDisable(GL_CULL_FACE);
			break;
		case RenderMode::Wireframe:
			glDisable(GL_CULL_FACE);
			break;
		case RenderMode::Shadow:
			glEnable(GL_CULL_FACE);
			glCullFace(GL_FRONT);
			break;
		}

		//draw scene geometry
		for (Model* model : models)
		{
			//select shader (and texture group)
			if (this->GetRenderMode() == RenderMode::Postprocess)
			{
				this->m_shader_program->Select(-1);
			}
			else
			{
				this->m_shader_program->Select((int)model->GetReference());
			}

			//set uniforms
			if (this->GetRenderMode() != RenderMode::Postprocess) //postprocessing is done in one draw call
			{
				this->SetShaderUniform("mdl_translate", glm::vec4(model->GetPosition(), 0.0f));
				this->SetShaderUniform("mdl_rotate", model->GetRotationMatrix());
				this->SetShaderUniform("mdl_scale", model->GetScaleMatrix());

				if (this->GetRenderMode() != RenderMode::Shadow) //standard render modes
				{
					Material& material = model->GetMaterial();
					//material
					this->SetShaderUniform("mat_diffuse", material.diffuse);
					this->SetShaderUniform("mat_specular", material.specular);
					this->SetShaderUniform("mat_specular_highlight", material.specular_highlight);

					//screen space reflections
					this->SetShaderUniform("mat_ssr_enabled", material.ssr_enabled);
					this->SetShaderUniform("mat_ssr_resolution", material.ssr.resolution);
					this->SetShaderUniform("mat_ssr_max_distance", material.ssr.max_cam_distance);
					this->SetShaderUniform("mat_ssr_max_cast_distance", material.ssr.cast_distance_limit);
					this->SetShaderUniform("mat_ssr_depth_acceptance", material.ssr.depth_acceptance);
					this->SetShaderUniform("mat_ssr_show_this", material.ssr.appear_in_ssr);
					this->SetShaderUniform("mat_ssr_refinements", material.ssr.refinements);

					//textures
					LoadedTexture texture;

					texture = this->GetEngine()->GetTexture(model->GetColourTexture().GetReference());
					texture.uniform_name = "colourTexture";
					this->m_shader_program->SetTexture((int)model->GetReference(), texture);

					texture = this->GetEngine()->GetTexture(model->GetNormalTexture().GetReference());
					texture.uniform_name = "normalTexture";
					this->m_shader_program->SetTexture((int)model->GetReference(), texture);

					texture = this->GetEngine()->GetTexture(model->GetSpecularTexture().GetReference());
					texture.uniform_name = "specularTexture";
					this->m_shader_program->SetTexture((int)model->GetReference(), texture);

					texture = this->GetEngine()->GetTexture(model->GetReflectionTexture().GetReference());
					texture.uniform_name = "reflectionIntensityTexture";
					this->m_shader_program->SetTexture((int)model->GetReference(), texture);

					texture = this->GetEngine()->GetTexture(model->GetSkyboxMaskTexture().GetReference());
					texture.uniform_name = "skyboxMaskTexture";
					this->m_shader_program->SetTexture((int)model->GetReference(), texture);

					//reflections
					std::vector<std::tuple<Reflection*, ReflectionMode>> reflections = material.reflections;
					for (int i = 0; i < (int)reflections.size(); i++)
					{
						Reflection* reflection = std::get<0>(reflections.at(i));
						ReflectionMode reflection_mode = std::get<1>(reflections.at(i));

						std::string prefix = "reflections[" + std::to_string(i) + "].";

						this->AddShaderUniformName(prefix + "position");
						this->SetShaderUniform(prefix + "position", reflection->GetPosition());

						this->AddShaderUniformName(prefix + "clip_near");
						this->SetShaderUniform(prefix + "clip_near", std::get<0>(reflection->GetClips()));
						
						this->AddShaderUniformName(prefix + "clip_far");
						this->SetShaderUniform(prefix + "clip_far", std::get<1>(reflection->GetClips()));

						this->AddShaderUniformName(prefix + "mode");
						this->SetShaderUniform(prefix + "mode", reflection_mode == ReflectionMode::Iterative ? 0 : 1);

						this->AddShaderUniformName(prefix + "iterations");
						this->SetShaderUniform(prefix + "iterations", reflection->GetIterations());

						this->AddShaderUniformNames({
							"reflection_cubemaps[" + std::to_string(i) + "]",
							"reflection_depth_cubemaps[" + std::to_string(i) + "]"
							});
						for (int j = 0; j < ENGINECANVAS_NUM_DATA_TEX; j++)
						{
							this->AddShaderUniformName("reflection_data_cubemaps[" + std::to_string((i * ENGINECANVAS_NUM_DATA_TEX) + j) + "]");
						}
						
						RenderTextureGroup reflection_output = this->GetEngine()->GetRenderTexture(reflection->GetReference());

						LoadedTexture texture;
						texture.type = reflection_output.type;

						texture.id = reflection_output.colour;
						texture.uniform_name = "reflection_cubemaps[" + std::to_string(i) + "]";
						this->m_shader_program->SetTexture((int)model->GetReference(), texture);

						texture.id = reflection_output.depth;
						texture.uniform_name = "reflection_depth_cubemaps[" + std::to_string(i) + "]";
						this->m_shader_program->SetTexture((int)model->GetReference(), texture);

						for (int j = 0; j < ENGINECANVAS_NUM_DATA_TEX; j++)
						{
							texture.id = reflection_output.data.at(j);
							texture.uniform_name = "reflection_data_cubemaps[" + std::to_string((i * ENGINECANVAS_NUM_DATA_TEX) + j) + "]";
							this->m_shader_program->SetTexture((int)model->GetReference(), texture);
						}
					}

					//shade mode 1 (wireframe)
					this->SetShaderUniform("mode1_colour", model->GetWireframeColour());
				}
			}

			//load geometry
			Engine::LoadedGeometry loaded_geometry = this->GetEngine()->BindVAO(model);

			//draw geometry
			glDrawArrays(GL_TRIANGLES, 0, loaded_geometry.num_vertices);

			if (dealloc_models) //release geometry as the models are temporary
			{
				this->GetEngine()->ReleaseVAO(model);
			}
		}

		//iterate through models
		// load model data into shader program
		// load model textures into shader program
		// draw model

		this->m_fbo_contains_render = true;

		if (dealloc_models)
		{
			for (int i = 0; i < (int)models.size(); i++)
			{
				delete models.at(i);
			}
		}
	}
}

void Renderable::RecompileShader()
{
	if (this->m_shader_program != nullptr)
	{
		delete this->m_shader_program;
	}

	std::vector<std::tuple<std::string, std::string>> shader_defines;
	for (std::map<std::string, std::string>::iterator it = this->m_shader_defines.begin(); it != this->m_shader_defines.end(); it++)
	{
		shader_defines.push_back({ it->first, it->second });
	}

	this->m_shader_program = new ShaderProgram(
		this->m_shaders,
		shader_defines,
		false
	);

	for (std::vector<std::string>::iterator it = this->m_shader_uniform_names.begin(); it != this->m_shader_uniform_names.end(); it++)
	{
		this->m_shader_program->RegisterUniform(*it);
	}
}

void Renderable::SetFramebuffer(GLuint fbo)
{
	this->m_fbo = fbo;
	this->m_fbo_contains_render = false;
}

GLuint Renderable::GetFramebuffer()
{
	return this->m_fbo;
}

void Renderable::SetTargetType(GLuint target_type)
{
	this->m_fbo_target_type = target_type;
}

bool Renderable::SetShaderDefine(std::string key, std::string value)
{
	std::map<std::string, std::string>::iterator it = this->m_shader_defines.find(key);
	if (it == this->m_shader_defines.end())
	{
		this->m_shader_defines.insert(std::pair(key, value));
		return true;
	}
	else if (it->second != value)
	{
		this->m_shader_defines.at("POINT_LIGHT_NUM") = value;
		return true;
	}
	return false;
}

void Renderable::AddShaderUniformName(std::string name)
{
	if (std::find(this->m_shader_uniform_names.begin(), this->m_shader_uniform_names.end(), name) == this->m_shader_uniform_names.end())
	{
		this->m_shader_uniform_names.push_back(name);

		if (this->m_shader_program != nullptr)
		{
			this->m_shader_program->RegisterUniform(name);
		}
	}
}

void Renderable::AddShaderUniformNames(std::vector<std::string> names)
{
	for (int i = 0; i < (int)names.size(); i++)
	{
		this->AddShaderUniformName(names.at(i));
	}
}

void Renderable::SetShaderUniform(std::string name, bool value)
{
	glUniform1i(this->m_shader_program->GetUniform(name), value);
}

void Renderable::SetShaderUniform(std::string name, int value)
{
	glUniform1i(this->m_shader_program->GetUniform(name), value);
}

void Renderable::SetShaderUniform(std::string name, float value)
{
	glUniform1f(this->m_shader_program->GetUniform(name), value);
}

void Renderable::SetShaderUniform(std::string name, double value, bool demote)
{
	if (demote)
	{
		this->SetShaderUniform(name, (float)value);
	}
	else
	{
		glUniform1d(this->m_shader_program->GetUniform(name), value);
	}
}

void Renderable::SetShaderUniform(std::string name, glm::vec3 vec)
{
	glUniform3fv(this->m_shader_program->GetUniform(name), 1, glm::value_ptr(vec));
}

void Renderable::SetShaderUniform(std::string name, glm::dvec3 vec, bool demote)
{
	if (demote)
	{
		this->SetShaderUniform(name, glm::vec3(vec));
	}
	else
	{
		glUniform3dv(this->m_shader_program->GetUniform(name), 1, glm::value_ptr(vec));
	}
}

void Renderable::SetShaderUniform(std::string name, glm::vec4 vec)
{
	glUniform4fv(this->m_shader_program->GetUniform(name), 1, glm::value_ptr(vec));
}

void Renderable::SetShaderUniform(std::string name, glm::dvec4 vec, bool demote)
{
	if (demote)
	{
		this->SetShaderUniform(name, glm::vec4(vec));
	}
	else
	{
		glUniform4dv(this->m_shader_program->GetUniform(name), 1, glm::value_ptr(vec));
	}
}

void Renderable::SetShaderUniform(std::string name, glm::mat4 mat)
{
	glUniformMatrix4fv(this->m_shader_program->GetUniform(name), 1, GL_FALSE, glm::value_ptr(mat));
}

void Renderable::SetShaderUniform(std::string name, glm::dmat4 mat, bool demote)
{
	if (demote)
	{
		this->SetShaderUniform(name, glm::mat4(mat));
	}
	else
	{
		glUniformMatrix4dv(this->m_shader_program->GetUniform(name), 1, GL_FALSE, glm::value_ptr(mat));
	}
}

void Renderable::ConfigureShader(RenderMode mode)
{
	if (mode != this->m_rendermode)
	{
		this->m_rendermode = mode;
		this->m_fbo_contains_render = false;

		if (mode == RenderMode::Normal)
		{
			this->m_shaders = {
				{ GetEmbeddedTextfile(RCID_TF_MODEL_FRAGSHADER), GL_FRAGMENT_SHADER },
				{ GetEmbeddedTextfile(RCID_TF_MODEL_GEOMSHADER_MODE0), GL_GEOMETRY_SHADER },
				{ GetEmbeddedTextfile(RCID_TF_MODEL_VERTSHADER), GL_VERTEX_SHADER }
			};
		}
		else if (mode == RenderMode::Wireframe)
		{
			this->m_shaders = {
				{ GetEmbeddedTextfile(RCID_TF_MODEL_FRAGSHADER), GL_FRAGMENT_SHADER },
				{ GetEmbeddedTextfile(RCID_TF_MODEL_GEOMSHADER_MODE1), GL_GEOMETRY_SHADER },
				{ GetEmbeddedTextfile(RCID_TF_MODEL_VERTSHADER), GL_VERTEX_SHADER }
			};
		}
		else if (mode == RenderMode::Shadow)
		{
			this->m_shaders = {
				{ GetEmbeddedTextfile(RCID_TF_MODEL_SHADOW_FRAGSHADER), GL_FRAGMENT_SHADER },
				{ GetEmbeddedTextfile(RCID_TF_MODEL_SHADOW_GEOMSHADER), GL_GEOMETRY_SHADER },
				{ GetEmbeddedTextfile(RCID_TF_MODEL_VERTSHADER), GL_VERTEX_SHADER }
			};
		}
		else if (mode == RenderMode::Postprocess)
		{
			this->m_shaders = {
				{ GetEmbeddedTextfile(RCID_TF_POSTPROCESS_FRAGSHADER), GL_FRAGMENT_SHADER },
				{ GetEmbeddedTextfile(RCID_TF_POSTPROCESS_VERTSHADER), GL_VERTEX_SHADER }
			};
		}

		this->AddShaderUniformNames({
			"cubemap_transform[0]",
			"cubemap_transform[1]",
			"cubemap_transform[2]",
			"cubemap_transform[3]",
			"cubemap_transform[4]",
			"cubemap_transform[5]",
			"is_cubemap"
			});

		if ((mode == RenderMode::Normal) || (mode == RenderMode::Wireframe))
		{
			this->AddShaderUniformNames({
				//vertex
				"mdl_translate",
				"mdl_rotate",
				"mdl_scale",
				"cam_translate",
				"cam_rotate",
				"cam_persp",
				"cam_clip_near",
				"cam_clip_far",
				"cam_transform",
				"cam_transform_inverse",
				//fragment
				"shade_mode",
				"mat_diffuse",
				"mat_specular",
				"mat_specular_highlight",
				"mat_ssr_enabled",
				"mat_ssr_resolution",
				"mat_ssr_max_distance",
				"mat_ssr_max_cast_distance",
				"mat_ssr_depth_acceptance",
				"mat_ssr_show_this",
				"mat_ssr_refinements",
				"colourTexture",
				"normalTexture",
				"specularTexture",
				"reflectionIntensityTexture",
				"light_ambient",
				"skyboxMaskTexture",
				"skyboxTexture",
				"render_output_valid",
				"render_output_colour",
				"render_output_depth",
				"render_output_x",
				"render_output_y"
				"mode1_colour"
				});

			for (int i = 0; i < ENGINECANVAS_NUM_DATA_TEX; i++)
			{
				this->AddShaderUniformName("render_output_data[" + std::to_string(i) + "]");
			}
		}
		else if (mode == RenderMode::Postprocess)
		{
			this->AddShaderUniformNames({
				//fragment
				"render_output"
				});
		}
		else if (mode == RenderMode::Shadow)
		{
			this->AddShaderUniformNames({
				//vertex
				"mdl_translate",
				"mdl_rotate",
				"mdl_scale",
				//fragment
				"light_position",
				"light_far_plane",
				"light_near_plane"
				});
		}

		this->RecompileShader();
	}
}

bool Renderable::FramebufferContainsRenderOutput()
{
	return this->m_fbo_contains_render;
}

void Renderable::PreRenderEvent()
{
}

void Renderable::PostRenderEvent()
{
}

Renderable::Renderable(Engine* engine, RenderMode mode) : m_engine(engine)
{
	this->m_engine->MakeContextCurrent();

	this->m_shaders = {
		{ GetEmbeddedTextfile(RCID_TF_DEFAULT_FRAGSHADER), GL_FRAGMENT_SHADER },
		{ GetEmbeddedTextfile(RCID_TF_DEFAULT_VERTSHADER), GL_VERTEX_SHADER }
	};
	this->RecompileShader();

	this->ConfigureShader(mode);
}

Renderable::~Renderable()
{
	delete this->m_shader_program;
}

void Renderable::SetCamera(Camera* camera)
{
	this->m_camera = camera;
}

Camera* Renderable::GetCamera()
{
	return this->m_camera;
}

Engine* Renderable::GetEngine()
{
	return this->m_engine;
}

void Renderable::SetRenderMode(NormalRenderModeData data)
{
	this->ConfigureShader(RenderMode::Normal);
	this->m_rendermode_data_normal = data;
}

void Renderable::SetRenderMode(WireframeRenderModeData data)
{
	this->ConfigureShader(RenderMode::Wireframe);
	this->m_rendermode_data_wireframe = data;
}

void Renderable::SetRenderMode(ShadowRenderModeData data)
{
	this->ConfigureShader(RenderMode::Shadow);
	this->m_rendermode_data_shadow = data;
}

void Renderable::SetRenderMode(PostProcessRenderModeData data)
{
	this->ConfigureShader(RenderMode::Postprocess);
	this->m_rendermode_data_postprocess = data;
}

void Renderable::Render(std::vector<Model*> models, bool continuous_draw)
{
	if (this->m_engine->GetScene() != nullptr)
	{
		if (continuous_draw)
		{
			this->PostRenderEvent();
		}
		this->PreRenderEvent();

		std::tuple<int, int> sizes = this->GetOutputSize();

		glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);
		glViewport(0, 0, std::get<0>(sizes), std::get<0>(sizes));
		this->RenderScene(models);

		if (!continuous_draw)
		{
			this->PostRenderEvent();
		}
	}
}

std::tuple<int, int> Renderable::GetOutputSize()
{
	throw std::logic_error("Method must be overridden");
}

RenderMode Renderable::GetRenderMode()
{
	return this->m_rendermode;
}