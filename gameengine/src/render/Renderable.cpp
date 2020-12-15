#include "Renderable.h"

#include <stdexcept>

#include "../scene/model/Model.h"
#include "../scene/model/geometry/Polygonal.h"
#include "../scene/model/geometry/Patch.h"
#include "../scene/Camera.h"
#include "../scene/Scene.h"
#include "../scene/light/PointLight.h"
#include "../scene/OrientedBoundingBox.h"
#include "../scene/Skybox.h"
#include "ShaderProgram.h"
#include "../Engine.h"
#include "../Resource.h"

void Renderable::RenderScene(std::vector<Model*> models)
{
	if (this->GetRenderMode() == RenderMode::Default)
	{
		throw std::runtime_error("Can't render when in default mode");
	}

	Scene* scene = this->m_engine->GetScene();
	if (scene != nullptr)
	{
#ifdef _DEBUG
		if ((this->m_fbo != 0) && !glIsFramebuffer(this->m_fbo))
		{
			throw std::runtime_error("FBO provided is not an FBO");
		}
#endif

		glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);

		//resolve models
		bool dealloc_models = false;
		{
			bool models_provided = !((models.size() == 1) && (models.at(0) == nullptr));
			if (!models_provided)
			{
				models.clear();
			}

			if (this->RenderModeIsModelRendering(this->GetRenderMode()))
			{
				if (!models_provided)
				{
					models = scene->GetModels();
				}

				models = scene->GetVisibleModels(this->m_camera->GetPosition(), this->GetRenderMode(), models);
			}
			else if (this->GetRenderMode() == RenderMode::Postprocess)
			{
				if (!models_provided) //default state
				{
					models.clear();

					std::shared_ptr<Polygonal> geom = std::make_shared<Polygonal>();

					Polygonal::Face face = Polygonal::Face(*geom);

					const double vertex_size = 1.0;
					const double uv_size = 1.0;
					face.AddVertex(Polygonal::Face::StandaloneVertex(glm::dvec3(-vertex_size, -vertex_size, 0.0), glm::dvec2(0.0, 0.0)));
					face.AddVertex(Polygonal::Face::StandaloneVertex(glm::dvec3(-vertex_size, vertex_size, 0.0), glm::dvec2(0.0, uv_size)));
					face.AddVertex(Polygonal::Face::StandaloneVertex(glm::dvec3(vertex_size, vertex_size, 0.0), glm::dvec2(uv_size, uv_size)));
					face.AddVertex(Polygonal::Face::StandaloneVertex(glm::dvec3(vertex_size, -vertex_size, 0.0), glm::dvec2(uv_size, 0.0)));

					face.SetNormal(glm::dvec3(0.0, 0.0, -1.0));

					geom->AddFace(face);

					models.push_back(new Model(-1, std::vector<std::shared_ptr<Geometry>>({ geom })));

					dealloc_models = true;
				}
			}
			else
			{
				throw std::runtime_error("Unknown render mode " + static_cast<int>(this->GetRenderMode()));
			}
		}

		//set preprocessor defines and recompile if any have changed
		bool recompile_required = false;

		if (this->GetRenderMode() == RenderMode::Normal)
		{
			//point lights
			recompile_required = this->SetShaderDefine("POINT_LIGHT_NUM", std::to_string(this->GetEngine()->GetScene()->GetPointLights().size())) ? true : recompile_required;

			//OBB approximations
			recompile_required = this->SetShaderDefine("APPROXIMATION_OBB_NUM", std::to_string(this->GetEngine()->GetScene()->GetOBBApproximations().size())) ? true : recompile_required;

			//reflections
			recompile_required = this->SetShaderDefine("REFLECTION_NUM", std::to_string(this->GetEngine()->GetScene()->GetReflections().size())) ? true : recompile_required;

			//determine if any models might need to discard fragments
			{
				bool frags_may_be_discarded = false;

				for (Model* model : models)
				{
					if (model->GetMaterial().displacement.discard_out_of_range)
					{
						frags_may_be_discarded = true;
					}
				}

				recompile_required = this->SetShaderDefine("SUPPORT_DISPLACEMENT_OUT_OF_RANGE_DISCARDING", frags_may_be_discarded ? "1" : "0") ? true : recompile_required;
			}
		}

		if ((this->GetRenderMode() == RenderMode::Normal)
			|| (this->GetRenderMode() == RenderMode::Wireframe)
			|| (this->GetRenderMode() == RenderMode::Textured))
		{
			//data textures
			recompile_required = this->SetShaderDefine("DATA_TEX_NUM", std::to_string(GAMEENGINE_NUM_DATA_TEX)) ? true : recompile_required;
		}

		if (this->GetRenderMode() == RenderMode::Postprocess)
		{
			size_t num_layers = std::get<RenderableConfig::PostProcess>(this->m_config.mode_data).layers.size();
			recompile_required = this->SetShaderDefine("COMPOSITE_LAYER_NUM", std::to_string(num_layers)) ? true : recompile_required;
		}

		if (recompile_required)
		{
			this->RecompileShader();
		}

		this->m_shader_program->Select();

		//load "constant" uniforms (uniforms constant between models like camera data) into program
		// cubemap uniforms
		{
			bool is_cubemap = this->GetTargetType() == GL_TEXTURE_CUBE_MAP;
			this->SetShaderUniform("is_cubemap", is_cubemap);

			if (is_cubemap)
			{
				std::vector<glm::mat4> transforms;
				glm::vec3 translate = glm::vec3(0.0f);

				transforms.push_back(glm::lookAt(translate, translate + glm::vec3(1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
				transforms.push_back(glm::lookAt(translate, translate + glm::vec3(-1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
				transforms.push_back(glm::lookAt(translate, translate + glm::vec3(0.0f, 1.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f)));
				transforms.push_back(glm::lookAt(translate, translate + glm::vec3(0.0f, -1.0f, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f)));
				transforms.push_back(glm::lookAt(translate, translate + glm::vec3(0.0f, 0.0f, 1.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
				transforms.push_back(glm::lookAt(translate, translate + glm::vec3(0.0f, 0.0f, -1.0f), glm::vec3(0.0f, -1.0f, 0.0f)));

				for (int i = 0; i < static_cast<int>(transforms.size()); i++)
				{
					this->SetShaderUniform("cubemap_transform[" + std::to_string(i) + "]", transforms.at(i));
				}
			}
			else
			{
				this->SetShaderUniform("cubemap_transform[0]", glm::mat4(1.0f));
			}
		}

		//specialised uniforms
		if (this->GetRenderMode() == RenderMode::Postprocess)
		{
			for (size_t i = 0; i < std::get<RenderableConfig::PostProcess>(this->m_config.mode_data).layers.size(); i++)
			{
				const RenderableConfig::PostProcess::CompositeLayer& layer = std::get<RenderableConfig::PostProcess>(this->m_config.mode_data).layers.at(i);

				LoadedTexture texture;
				texture.id = layer.id;
				texture.type = GL_TEXTURE_2D;
				texture.uniform_name = "layers_texture[" + std::to_string(i) + "]";

				this->m_shader_program->SetTexture(-1, texture);

				const std::string prefix = "layers[" + std::to_string(i) + "].";
				this->SetShaderUniform(prefix + "colour_translate", layer.colour_translate);
				this->SetShaderUniform(prefix + "colour_scale", layer.colour_scale);
			}
			
		}
		else if (this->RenderModeIsModelRendering(this->GetRenderMode()))
		{
			// camera
			this->SetShaderUniform("cam_translate", glm::vec4(0.0 - this->GetCamera()->GetPosition(), 0.0f));
			this->SetShaderUniform("cam_rotate", this->GetCamera()->GetRotationMatrixInverse());
			this->SetShaderUniform("cam_persp", this->GetCamera()->GetPerspectiveMatrix());
			this->SetShaderUniform("cam_clip_near", std::get<0>(this->GetCamera()->GetClips()));
			this->SetShaderUniform("cam_clip_far", std::get<1>(this->GetCamera()->GetClips()));
			this->SetShaderUniform("cam_transform", this->GetCamera()->GetCombinedMatrix());
			this->SetShaderUniform("cam_transform_inverse", glm::inverse(this->GetCamera()->GetCombinedMatrix()));
		}
		else
		{
			throw std::runtime_error("Unknown render mode " + static_cast<int>(this->GetRenderMode()));
		}

		if (this->GetRenderMode() == RenderMode::Normal)
		{
			// ambient light
			this->SetShaderUniform("light_ambient", scene->GetAmbientLight());

			// shadows
			bool shadows_enabled = true;
			if (!std::get<RenderableConfig::Normal>(this->m_config.mode_data).draw_shadows)
			{
				shadows_enabled = false;
			}
			this->SetShaderUniform("light_shadow_draw", shadows_enabled);

			//point lights
			std::vector<PointLight*> point_lights = this->GetEngine()->GetScene()->GetPointLights();
			for (int i = 0; i < static_cast<int>(point_lights.size()); i++)
			{
				PointLight* point_light = point_lights.at(i);
				std::string root_name = "light_points[" + std::to_string(i) + "].";

				this->AddShaderUniformName(root_name + "position");
				this->SetShaderUniform(root_name + "position", point_light->GetPosition());

				this->AddShaderUniformName(root_name + "intensity");
				this->SetShaderUniform(root_name + "intensity", point_light->GetIntensity());

				this->AddShaderUniformName(root_name + "shadow_far_plane");
				this->SetShaderUniform(root_name + "shadow_far_plane", std::get<1>(point_light->GetClips()));

				this->AddShaderUniformName(root_name + "shadow_bias");
				this->SetShaderUniform(root_name + "shadow_bias", point_light->GetShadowBias());
				
				std::string cubemap_name = "light_shadow_cubemaps[" + std::to_string(i) + "]";

				this->AddShaderUniformName(cubemap_name);
				RenderTextureGroup texture = this->GetEngine()->GetRenderTexture(point_light->GetReference());

				LoadedTexture loaded_texture;
				loaded_texture.id = texture.depth;
				loaded_texture.type = GL_TEXTURE_CUBE_MAP;
				loaded_texture.uniform_name = cubemap_name;
				
				this->m_shader_program->SetTexture(-1, loaded_texture);
			}

			//scene approximation
			std::vector<OrientedBoundingBox> scene_approximations = this->GetEngine()->GetScene()->GetOBBApproximations();
			for (int i = 0; i < static_cast<int>(scene_approximations.size()); i++)
			{
				OrientedBoundingBox obb = scene_approximations.at(i);
				std::string prefix = "scene_approximations[" + std::to_string(i) + "].";

				this->AddShaderUniformName(prefix + "position");
				this->SetShaderUniform(prefix + "position", obb.GetPosition());
				
				this->AddShaderUniformName(prefix + "dimensions");
				this->SetShaderUniform(prefix + "dimensions", obb.GetDimensionsVec());

				this->AddShaderUniformName(prefix + "rotation");
				this->SetShaderUniform(prefix + "rotation", glm::mat3(obb.GetRotationMatrix()));

				this->AddShaderUniformName(prefix + "rotation_inverse");
				this->SetShaderUniform(prefix + "rotation_inverse", glm::mat3(obb.GetRotationMatrixInverse()));
			}

			//previous render result
			if (this->GetTargetType() == GL_TEXTURE_CUBE_MAP)
			{
				this->SetShaderUniform("render_output_valid", false);
			}
			else
			{
				this->SetShaderUniform("render_output_valid", this->FramebufferContainsRenderOutput());
			}

			this->SetShaderUniform("render_output_x", std::get<0>(this->GetOutputSize()));
			this->SetShaderUniform("render_output_y", std::get<1>(this->GetOutputSize()));

			//load textures from the previous frame (if in normal rendering mode)
			LoadedTexture texture;
			texture.type = std::get<RenderableConfig::Normal>(this->m_config.mode_data).previous_frame.type;
			texture.id = std::get<RenderableConfig::Normal>(this->m_config.mode_data).previous_frame.colour;
			texture.uniform_name = "render_output_colour";
			this->m_shader_program->SetTexture(-1, texture);

			texture.id = std::get<RenderableConfig::Normal>(this->m_config.mode_data).previous_frame.depth;
			texture.uniform_name = "render_output_depth";
			this->m_shader_program->SetTexture(-1, texture);

			for (int i = 0; i < static_cast<int>(std::get<RenderableConfig::Normal>(this->m_config.mode_data).previous_frame.data.size()); i++)
			{
				texture.id = std::get<RenderableConfig::Normal>(this->m_config.mode_data).previous_frame.data.at(i);
				texture.uniform_name = "render_output_data[" + std::to_string(i) + "]";
				this->m_shader_program->SetTexture(-1, texture);
			}
		}

		if (this->GetRenderMode() == RenderMode::Wireframe)
		{
			this->SetShaderUniform("draw_back_faces", std::get<RenderableConfig::Wireframe>(this->m_config.mode_data).draw_back_faces);
		}

		switch (this->GetRenderMode())
		{
		case RenderMode::Normal:
			glEnable(GL_CULL_FACE);
			glCullFace(GL_BACK);

			glEnable(GL_DEPTH_TEST);
			break;
		case RenderMode::Postprocess:
			glDisable(GL_CULL_FACE);

			glDisable(GL_DEPTH_TEST);
			break;
		case RenderMode::Wireframe:
			glDisable(GL_CULL_FACE);

			glEnable(GL_DEPTH_TEST);
			break;
		case RenderMode::Shadow:
			glEnable(GL_CULL_FACE);
			glCullFace(GL_FRONT);

			glEnable(GL_DEPTH_TEST);
			break;
		case RenderMode::Textured:
			glEnable(GL_CULL_FACE);
			glCullFace(GL_BACK);

			glEnable(GL_DEPTH_TEST);
			break;
		}

		//prepare viewport
		glViewport(0, 0, std::get<0>(this->GetOutputSize()), std::get<1>(this->GetOutputSize()));
		glClearColor(
			scene->GetClearColour().r,
			scene->GetClearColour().g,
			scene->GetClearColour().b,
			scene->GetClearColour().a
		);

		glClearDepth(1.0);
		glDepthMask(GL_TRUE);

		if (this->m_config.clear_fbo)
		{
			if (this->GetRenderMode() == RenderMode::Shadow)
			{
				glClear(GL_DEPTH_BUFFER_BIT);
			}
			else
			{
				glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			}
		}

		//draw scene geometry
		for (Model* model : models)
		{
			//set uniforms
			if (this->GetRenderMode() == RenderMode::Postprocess)
			{
				this->m_shader_program->Select(-1); //select shader (and texture group) (this happens at the end of the other branch)
			}
			else
			{
				this->SetShaderUniform("mdl_translate", glm::vec4(model->GetPosition(), 0.0f));
				this->SetShaderUniform("mdl_rotate", model->GetRotationMatrix());
				this->SetShaderUniform("mdl_scale", model->GetScaleMatrix());

				if ((this->GetRenderMode() == RenderMode::Normal)
					|| (this->GetRenderMode() == RenderMode::Textured))
				{
					LoadedTexture texture = this->GetEngine()->GetTexture(model->GetColourTexture().GetReference());
					texture.uniform_name = "colourTexture";
					this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), texture);
				}

				if (this->GetRenderMode() == RenderMode::Normal)
				{
					Material& material = model->GetMaterial();
					//material
					this->SetShaderUniform("mat_diffuse", material.diffuse);
					this->SetShaderUniform("mat_specular", material.specular);
					this->SetShaderUniform("mat_specular_highlight", material.specular_highlight);
					this->SetShaderUniform("mat_displacement_multiplier", material.displacement.multiplier);
					this->SetShaderUniform("mat_displacement_discard_out_of_range", material.displacement.discard_out_of_range);

					//screen space reflections
					this->SetShaderUniform("mat_ssr_enabled", material.ssr_enabled);
					this->SetShaderUniform("mat_ssr_resolution", material.ssr.resolution);
					this->SetShaderUniform("mat_ssr_max_distance", material.ssr.max_cam_distance);
					this->SetShaderUniform("mat_ssr_max_cast_distance", material.ssr.cast_distance_limit);
					this->SetShaderUniform("mat_ssr_depth_acceptance", material.ssr .depth_acceptance);
					this->SetShaderUniform("mat_ssr_show_this", material.ssr.appear_in_ssr);
					this->SetShaderUniform("mat_ssr_refinements", material.ssr.refinements);

					//textures
					LoadedTexture texture;

					texture = this->GetEngine()->GetTexture(model->GetNormalTexture().GetReference());
					texture.uniform_name = "normalTexture";
					this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), texture);

					texture = this->GetEngine()->GetTexture(model->GetSpecularTexture().GetReference());
					texture.uniform_name = "specularTexture";
					this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), texture);

					texture = this->GetEngine()->GetTexture(model->GetReflectionTexture().GetReference());
					texture.uniform_name = "reflectionIntensityTexture";
					this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), texture);

					texture = this->GetEngine()->GetTexture(model->GetSkyboxMaskTexture().GetReference());
					texture.uniform_name = "skyboxMaskTexture";
					this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), texture);

					texture = this->GetEngine()->GetTexture(model->GetDisplacementTexture().GetReference());
					texture.uniform_name = "displacementTexture";
					this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), texture);

					//reflections
					this->SetShaderUniform("reflections_enabled", material.reflections_enabled);

					std::vector<std::tuple<Reflection*, ReflectionMode>> reflections = material.reflections;
					for (int i = 0; i < static_cast<int>(reflections.size()); i++)
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
						this->SetShaderUniform(prefix + "mode", static_cast<int>(reflection_mode));

						this->AddShaderUniformName(prefix + "iterations");
						this->SetShaderUniform(prefix + "iterations", reflection->GetIterations());

						this->AddShaderUniformNames({
							"reflection_cubemaps[" + std::to_string(i) + "]",
							"reflection_depth_cubemaps[" + std::to_string(i) + "]"
							});
						for (int j = 0; j < GAMEENGINE_NUM_DATA_TEX; j++)
						{
							this->AddShaderUniformName("reflection_data_cubemaps[" + std::to_string((i * GAMEENGINE_NUM_DATA_TEX) + j) + "]");
						}
						
						RenderTextureGroup reflection_output = this->GetEngine()->GetRenderTexture(reflection->GetReference());

						LoadedTexture texture;
						texture.type = reflection_output.type;

						texture.id = reflection_output.colour;
						texture.uniform_name = "reflection_cubemaps[" + std::to_string(i) + "]";
						this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), texture);

						texture.id = reflection_output.depth;
						texture.uniform_name = "reflection_depth_cubemaps[" + std::to_string(i) + "]";
						this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), texture);

						for (int j = 0; j < GAMEENGINE_NUM_DATA_TEX; j++)
						{
							texture.id = reflection_output.data.at(j);
							texture.uniform_name = "reflection_data_cubemaps[" + std::to_string((i * GAMEENGINE_NUM_DATA_TEX) + j) + "]";
							this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), texture);
						}
					}

					this->SetShaderUniform("reflection_count", static_cast<int>(reflections.size()));

					//skybox cubemap
					Skybox* skybox = model->GetSkybox();
					if (skybox != nullptr)
					{
						RenderTextureGroup render_texture = this->GetEngine()->GetRenderTexture(skybox->GetReference());
						LoadedTexture loaded_texture;
						loaded_texture.id = render_texture.colour;
						loaded_texture.type = render_texture.type;
						loaded_texture.uniform_name = "skyboxTexture";
						this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), loaded_texture);
					}
				}

				if (this->GetRenderMode() == RenderMode::Wireframe)
				{
					this->SetShaderUniform("wireframe_colour", model->GetCurrentWireframeColour());
				}

				this->m_shader_program->Select(static_cast<int>(model->GetReference())); //select shader (and texture group)
			}

			//load geometry
			std::vector<std::shared_ptr<Geometry>> geometries = model->GetGeometry();
			for (const std::shared_ptr<Geometry>& geometry : geometries)
			{
				Engine::LoadedGeometry loaded_geometry = this->GetEngine()->BindVAO(model, geometry);

				//draw geometry
				GLenum mode = GL_NONE; //default invalid value, should be overwritten

				Geometry::PrimitiveType primitive_type = loaded_geometry.source->GetPrimitiveType();
				if (primitive_type == Geometry::PrimitiveType::Triangles)
				{
					this->SetShaderUniform("tess_enable", false);

					if (this->RenderModeIsModelRendering(this->GetRenderMode()))
					{
						mode = GL_PATCHES;
						glPatchParameteri(GL_PATCH_VERTICES, static_cast<GLint>(3));
					}
					else
					{
						mode = GL_TRIANGLES;
					}
				}
				else if (primitive_type == Geometry::PrimitiveType::Patch)
				{
					this->SetShaderUniform("tess_enable", true);

					mode = GL_PATCHES;

					std::size_t patch_size = geometry->GetPrimitivesNumValues() / static_cast<std::size_t>(GAMEENGINE_VALUES_PER_VERTEX);

#ifdef _DEBUG
					GLint max_patch_size = 32; //this is the minimum value required by the standard
					glGetIntegerv(GL_MAX_PATCH_VERTICES, &max_patch_size);

					//the range is open at this end, so the size of the patch must always be at least 1 less than the value returned
					//I just decrement the returned value so that it behaves "as it should" instead of dealing with this
					max_patch_size--;

					if (static_cast<std::size_t>(max_patch_size) < patch_size)
					{
						throw std::runtime_error("Patch provided has " + std::to_string(patch_size) + " vertices, but the implementation defined maximum is " + std::to_string(static_cast<int>(max_patch_size)));
					}
#endif

					glPatchParameteri(GL_PATCH_VERTICES, static_cast<GLint>(patch_size));
				}
				else
				{
					throw std::runtime_error("Unknown primitive type \"" + std::to_string(static_cast<int>(primitive_type)) + "\"");
				}

				glDrawArrays(mode, 0, static_cast<GLsizei>(loaded_geometry.data.size() / static_cast<std::size_t>(GAMEENGINE_VALUES_PER_VERTEX)));
			}

			if (dealloc_models) //release geometry as the models are temporary
			{
				this->GetEngine()->ReleaseVAOs(model);
			}
		}

		this->m_fbo_contains_render = true;

		if (dealloc_models)
		{
			for (size_t i = 0; i < models.size(); i++)
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

	for (std::set<std::string>::iterator it = this->m_shader_uniform_names.begin(); it != this->m_shader_uniform_names.end(); it++)
	{
		this->m_shader_program->RegisterUniform(*it);
	}
}

void Renderable::SetFramebuffer(GLuint fbo)
{
	this->m_fbo = fbo;
	this->m_fbo_contains_render = false;
}

GLuint Renderable::GetFramebuffer() const
{
	return this->m_fbo;
}

void Renderable::SetTargetType(GLenum target_type)
{
	this->m_fbo_target_type = target_type;
}

GLenum Renderable::GetTargetType() const
{
	return this->m_fbo_target_type;
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
		this->m_shader_defines.at(key) = value;
		return true;
	}
	return false;
}

void Renderable::AddShaderUniformName(std::string name)
{
	std::pair<std::set<std::string>::iterator, bool> insert_result = this->m_shader_uniform_names.insert(name);

	if (insert_result.second && (this->m_shader_program != nullptr))
	{
		this->m_shader_program->RegisterUniform(name);
	}
}

void Renderable::AddShaderUniformNames(std::vector<std::string> names)
{
	for (std::string name : names)
	{
		this->AddShaderUniformName(name);
	}
}

void Renderable::SetShaderUniform(std::string name, bool value)
{
	glUniform1i(this->m_shader_program->GetUniform(name), value ? GL_TRUE : GL_FALSE);
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

void Renderable::SetShaderUniform(std::string name, glm::mat3 mat)
{
	glUniformMatrix3fv(this->m_shader_program->GetUniform(name), 1, GL_FALSE, glm::value_ptr(mat));
}

void Renderable::SetShaderUniform(std::string name, glm::dmat3 mat, bool demote)
{
	if (demote)
	{
		this->SetShaderUniform(name, glm::mat3(mat));
	}
	else
	{
		glUniformMatrix3dv(this->m_shader_program->GetUniform(name), 1, GL_FALSE, glm::value_ptr(mat));
	}
}

bool Renderable::FramebufferContainsRenderOutput() const
{
	return this->m_fbo_contains_render;
}

void Renderable::PreRenderEvent()
{
}

void Renderable::PostRenderEvent()
{
}

bool Renderable::RenderModeIsModelRendering(RenderMode mode)
{
	return (mode == RenderMode::Normal)
		|| (mode == RenderMode::Shadow)
		|| (mode == RenderMode::Wireframe)
		|| (mode == RenderMode::Textured);
}

Renderable::Renderable(Engine* engine, RenderableConfig config) : m_engine(engine), m_config(config)
{
	this->m_engine->MakeContextCurrent();

	this->m_render_function = [this](std::vector<Model*> models)
	{
		this->RenderScene(models);
	};

	this->SetConfig(config);
}

Renderable::~Renderable()
{
	delete this->m_shader_program;
}

void Renderable::SetCamera(Camera* camera)
{
	this->m_camera = camera;
}

Camera* Renderable::GetCamera() const
{
	return this->m_camera;
}

Engine* Renderable::GetEngine() const
{
	return this->m_engine;
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

		this->m_render_function(models);

		if (!continuous_draw)
		{
			this->PostRenderEvent();
		}
	}
}

void Renderable::SetRenderFunction(RenderableControllerFunction function)
{
	this->m_render_function = function;
}

RenderMode Renderable::GetRenderMode() const
{
	return this->m_config.mode;
}

void Renderable::SetConfig(RenderableConfig config)
{
	this->m_config = config;
	this->m_fbo_contains_render = false;

	this->m_shaders.clear();
	if (this->RenderModeIsModelRendering(this->GetRenderMode()))
	{
		this->m_shaders.push_back(std::tuple(GetEmbeddedTextfile(RCID_TF_MODEL_VERTSHADER), GL_VERTEX_SHADER));
		this->m_shaders.push_back(std::tuple(GetEmbeddedTextfile(RCID_TF_MODEL_TESSCONTROLSHADER), GL_TESS_CONTROL_SHADER));
		this->m_shaders.push_back(std::tuple(GetEmbeddedTextfile(RCID_TF_MODEL_TESSEVALSHADER), GL_TESS_EVALUATION_SHADER));

		if (this->GetRenderMode() == RenderMode::Normal)
		{
			this->m_shaders.push_back(std::tuple(GetEmbeddedTextfile(RCID_TF_MODEL_GEOMSHADER), GL_GEOMETRY_SHADER));
			this->m_shaders.push_back(std::tuple(GetEmbeddedTextfile(RCID_TF_MODEL_NORMAL_FRAGSHADER), GL_FRAGMENT_SHADER));
		}
		else if (this->GetRenderMode() == RenderMode::Wireframe)
		{
			this->m_shaders.push_back(std::tuple(GetEmbeddedTextfile(RCID_TF_MODEL_WIREFRAME_GEOMSHADER), GL_GEOMETRY_SHADER));
			this->m_shaders.push_back(std::tuple(GetEmbeddedTextfile(RCID_TF_MODEL_WIREFRAME_FRAGSHADER), GL_FRAGMENT_SHADER));
		}
		else if (this->GetRenderMode() == RenderMode::Shadow)
		{
			this->m_shaders.push_back(std::tuple(GetEmbeddedTextfile(RCID_TF_MODEL_GEOMSHADER), GL_GEOMETRY_SHADER));
			this->m_shaders.push_back(std::tuple(GetEmbeddedTextfile(RCID_TF_MODEL_SHADOW_FRAGSHADER), GL_FRAGMENT_SHADER));
		}
		else if (this->GetRenderMode() == RenderMode::Textured)
		{
			this->m_shaders.push_back(std::tuple(GetEmbeddedTextfile(RCID_TF_MODEL_GEOMSHADER), GL_GEOMETRY_SHADER));
			this->m_shaders.push_back(std::tuple(GetEmbeddedTextfile(RCID_TF_MODEL_TEXTURED_FRAGSHADER), GL_FRAGMENT_SHADER));
		}
	}
	else if (this->GetRenderMode() == RenderMode::Postprocess)
	{
		this->m_shaders = {
			{ GetEmbeddedTextfile(RCID_TF_POSTPROCESS_FRAGSHADER), GL_FRAGMENT_SHADER },
			{ GetEmbeddedTextfile(RCID_TF_POSTPROCESS_VERTSHADER), GL_VERTEX_SHADER }
		};
	}
	else if (this->GetRenderMode() == RenderMode::Default)
	{
		this->m_shaders = {
			{ GetEmbeddedTextfile(RCID_TF_DEFAULT_FRAGSHADER), GL_FRAGMENT_SHADER },
			{ GetEmbeddedTextfile(RCID_TF_DEFAULT_VERTSHADER), GL_VERTEX_SHADER }
		};
	}

	this->m_shader_uniform_names.clear();

	this->RecompileShader();

	if (this->RenderModeIsModelRendering(this->GetRenderMode()))
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
			//tesselation
			"tess_enable",
			//geometry
			"cubemap_transform[0]",
			"cubemap_transform[1]",
			"cubemap_transform[2]",
			"cubemap_transform[3]",
			"cubemap_transform[4]",
			"cubemap_transform[5]",
			"is_cubemap"
			});

		if (this->GetRenderMode() == RenderMode::Normal)
		{
			this->AddShaderUniformNames({
				//fragment
				"mat_diffuse",
				"mat_specular",
				"mat_specular_highlight",
				"mat_displacement_multiplier",
				"mat_displacement_discard_out_of_range",
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
				"displacementTexture",
				"light_ambient",
				"light_shadow_draw",
				"reflections_enabled",
				"reflection_count",
				"skyboxMaskTexture",
				"skyboxTexture",
				"render_output_valid",
				"render_output_colour",
				"render_output_depth",
				"render_output_x",
				"render_output_y"
				});

			for (int i = 0; i < GAMEENGINE_NUM_DATA_TEX; i++)
			{
				this->AddShaderUniformName("render_output_data[" + std::to_string(i) + "]");
			}
		}
		else if (this->GetRenderMode() == RenderMode::Wireframe)
		{
			this->AddShaderUniformNames({
				//geometry
				"draw_back_faces",
				//fragment
				"wireframe_colour"
				});
		}
		else if (this->GetRenderMode() == RenderMode::Textured)
		{
			this->AddShaderUniformNames({
				//fragment
				"colourTexture"
				});
		}
	}
}

RenderableConfig Renderable::GetConfig() const
{
	return this->m_config;
}
