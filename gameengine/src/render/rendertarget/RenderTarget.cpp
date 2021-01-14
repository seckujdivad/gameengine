#include "RenderTarget.h"

#include <stdexcept>
#include <string>

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "../ShaderProgram.h"
#include "../../Engine.h"
#include "../../Resource.h"
#include "../../scene/Camera.h"
#include "../../scene/Scene.h"
#include "../../scene/OrientedBoundingBox.h"
#include "../../scene/Skybox.h"
#include "../../scene/light/PointLight.h"
#include "../../scene/model/Model.h"
#include "../../scene/model/geometry/PresetGeometry.h"
#include "../../scene/model/geometry/Patch.h"

#include "RenderTexture.h"

void RenderTarget::RenderScene(std::vector<Model*> models)
{
	if (this->GetRenderMode() == RenderTargetMode::Default)
	{
		throw std::runtime_error("Can't render when in default mode");
	}

	if (this->m_engine->GetScene() != nullptr)
	{
#ifdef _DEBUG
		if ((this->m_fbo != NULL) && !glIsFramebuffer(this->m_fbo))
		{
			throw std::runtime_error("FBO provided is not an FBO");
		}
#endif

		glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);

		if (this->RenderModeIsModelRendering())
		{
			models = this->Render_GetModels_Model(models);
		}
		else
		{
			models.clear();
			models.push_back(this->m_postprocess_model.get());
		}

		this->m_shader_program->Select();

		if (this->RenderModeIsModelRendering())
		{
			this->Render_Setup_Model(models);
		}
		else
		{
			this->Render_Setup_FlatQuad();
		}

		switch (this->GetRenderMode())
		{
		case RenderTargetMode::Normal:
			glEnable(GL_CULL_FACE);
			glCullFace(GL_BACK);

			glEnable(GL_DEPTH_TEST);
			break;
		case RenderTargetMode::Postprocess:
			glDisable(GL_CULL_FACE);

			glDisable(GL_DEPTH_TEST);
			break;
		case RenderTargetMode::Wireframe:
			glDisable(GL_CULL_FACE);

			glEnable(GL_DEPTH_TEST);
			break;
		case RenderTargetMode::Shadow:
			glEnable(GL_CULL_FACE);
			glCullFace(GL_FRONT);

			glEnable(GL_DEPTH_TEST);
			break;
		case RenderTargetMode::Textured:
			glEnable(GL_CULL_FACE);
			glCullFace(GL_BACK);

			glEnable(GL_DEPTH_TEST);
			break;
		}

		//prepare viewport
		glViewport(0, 0, std::get<0>(this->GetOutputSize()), std::get<1>(this->GetOutputSize()));

		glm::vec4 clear_colour = this->m_engine->GetScene()->GetClearColour();
		glClearColor(
			clear_colour.r,
			clear_colour.g,
			clear_colour.b,
			clear_colour.a
		);

		glClearDepth(1.0);
		glDepthMask(GL_TRUE);

		if (this->m_config.clear_fbo)
		{
			if (this->GetRenderMode() == RenderTargetMode::Shadow)
			{
				glClear(GL_DEPTH_BUFFER_BIT);
			}
			else
			{
				glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			}
		}

		const std::function<GLenum(Geometry::RenderInfo info, const Engine::LoadedGeometry& loaded_geometry)> predraw = [this](Geometry::RenderInfo info, const Engine::LoadedGeometry& loaded_geometry)
		{
			GLenum mode = GL_NONE; //default invalid value, should be overwritten

			if (this->RenderModeIsModelRendering())
			{
				if ((info.primitive_type == Geometry::PrimitiveType::Patches) || (info.primitive_type == Geometry::PrimitiveType::Quads))
				{
					mode = GL_PATCHES;

					this->m_shader_program->SetUniform("tess_enable", info.tesselation_enabled);
					this->m_shader_program->SetUniform("tess_interp_mode", static_cast<int>(info.interpolation_mode));

					this->m_shader_program->SetUniform("patch_size_u", info.primitive_dimensions.x);
					this->m_shader_program->SetUniform("patch_size_v", info.primitive_dimensions.y);
				}
				else
				{
					throw std::runtime_error("Primitive type " + std::to_string(static_cast<int>(info.primitive_type)) + " is not supported by model-oriented rendering modes");
				}
			}
			else if (this->GetRenderMode() == RenderTargetMode::Postprocess)
			{
				if (info.primitive_type == Geometry::PrimitiveType::Triangles)
				{
					mode = GL_TRIANGLES;
				}
				else
				{
					throw std::runtime_error("The only primitive type supported by postprocess rendering is triangles");
				}
			}
			else
			{
				throw std::runtime_error("Render mode " + std::to_string(static_cast<int>(this->GetRenderMode())) + " can't render geometry");
			}

			return mode;
		};

		//draw scene geometry
		for (Model* model : models)
		{
			if (this->RenderModeIsModelRendering())
			{
				this->Render_ForEachModel_Model(model);
			}
			else
			{
				this->Render_ForEachModel_Quad(model);
			}

			this->GetEngine()->DrawModel(model, predraw);
		}

		this->m_fbo_contains_render = true;
	}
}

void RenderTarget::SetFramebuffer(GLuint fbo)
{
	this->m_fbo = fbo;
	this->m_fbo_contains_render = false;
}

GLuint RenderTarget::GetFramebuffer() const
{
	return this->m_fbo;
}

void RenderTarget::SetTargetType(GLenum target_type)
{
	this->m_fbo_target_type = target_type;
}

GLenum RenderTarget::GetTargetType() const
{
	return this->m_fbo_target_type;
}

bool RenderTarget::FramebufferContainsRenderOutput() const
{
	return this->m_fbo_contains_render;
}

void RenderTarget::PreRenderEvent()
{
}

void RenderTarget::PostRenderEvent()
{
}

bool RenderTarget::RenderModeIsModelRendering(RenderTargetMode mode)
{
	return (mode == RenderTargetMode::Normal)
		|| (mode == RenderTargetMode::Shadow)
		|| (mode == RenderTargetMode::Wireframe)
		|| (mode == RenderTargetMode::Textured);
}

bool RenderTarget::RenderModeIsModelRendering()
{
	return this->RenderModeIsModelRendering(this->GetRenderMode());
}

std::vector<Model*> RenderTarget::Render_GetModels_Model(std::vector<Model*> model_pool)
{
	return this->GetEngine()->GetScene()->GetVisibleModels(this->m_camera->GetPosition(), this->GetRenderMode(), model_pool);
}

void RenderTarget::Render_Setup_Model(std::vector<Model*> models)
{
	bool recompile_required = false;
	
	if (this->GetRenderMode() == RenderTargetMode::Normal)
	{
		//point lights
		recompile_required = this->m_shader_program->SetDefine("POINT_LIGHT_NUM", std::to_string(this->GetEngine()->GetScene()->GetPointLights().size())) ? true : recompile_required;

		//OBB approximations
		recompile_required = this->m_shader_program->SetDefine("APPROXIMATION_OBB_NUM", std::to_string(this->GetEngine()->GetScene()->GetOBBApproximations().size())) ? true : recompile_required;

		//reflections
		recompile_required = this->m_shader_program->SetDefine("REFLECTION_NUM", std::to_string(this->GetEngine()->GetScene()->GetReflections().size())) ? true : recompile_required;

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

			recompile_required = this->m_shader_program->SetDefine("SUPPORT_DISPLACEMENT_OUT_OF_RANGE_DISCARDING", frags_may_be_discarded ? "1" : "0") ? true : recompile_required;
		}
	}

	if ((this->GetRenderMode() == RenderTargetMode::Normal)
		|| (this->GetRenderMode() == RenderTargetMode::Wireframe)
		|| (this->GetRenderMode() == RenderTargetMode::Textured))
	{
		//data textures
		recompile_required = this->m_shader_program->SetDefine("DATA_TEX_NUM", std::to_string(GAMEENGINE_NUM_DATA_TEX)) ? true : recompile_required;
	}

	if (recompile_required)
	{
		this->m_shader_program->Recompile();
	}

	//load "constant" uniforms (uniforms constant between models like camera data) into program
	// cubemap uniforms
	bool is_cubemap = this->GetTargetType() == GL_TEXTURE_CUBE_MAP;
	this->m_shader_program->SetUniform("is_cubemap", is_cubemap);

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
			this->m_shader_program->SetUniform("cubemap_transform[" + std::to_string(i) + "]", transforms.at(i));
		}
	}
	else
	{
		this->m_shader_program->SetUniform("cubemap_transform[0]", glm::mat4(1.0f));
	}

	// camera
	this->m_shader_program->SetUniform("cam_translate", glm::vec4(0.0 - this->GetCamera()->GetPosition(), 0.0f));
	this->m_shader_program->SetUniform("cam_rotate", this->GetCamera()->GetRotationMatrixInverse());
	this->m_shader_program->SetUniform("cam_persp", this->GetCamera()->GetPerspectiveMatrix());
	this->m_shader_program->SetUniform("cam_clip_near", std::get<0>(this->GetCamera()->GetClips()));
	this->m_shader_program->SetUniform("cam_clip_far", std::get<1>(this->GetCamera()->GetClips()));
	this->m_shader_program->SetUniform("cam_transform", this->GetCamera()->GetCombinedMatrix());
	this->m_shader_program->SetUniform("cam_transform_inverse", glm::inverse(this->GetCamera()->GetCombinedMatrix()));

	if (this->GetRenderMode() == RenderTargetMode::Normal)
	{
		// ambient light
		this->m_shader_program->SetUniform("light_ambient", this->GetEngine()->GetScene()->GetAmbientLight());

		// shadows
		bool shadows_enabled = true;
		if (!std::get<RenderTargetConfig::Normal>(this->m_config.mode_data).draw_shadows)
		{
			shadows_enabled = false;
		}
		this->m_shader_program->SetUniform("light_shadow_draw", shadows_enabled);

		//point lights
		std::vector<PointLight*> point_lights = this->GetEngine()->GetScene()->GetPointLights();
		for (int i = 0; i < static_cast<int>(point_lights.size()); i++)
		{
			PointLight* point_light = point_lights.at(i);
			std::string root_name = "light_points[" + std::to_string(i) + "].";
			this->m_shader_program->SetUniform(root_name + "position", point_light->GetPosition());
			this->m_shader_program->SetUniform(root_name + "intensity", point_light->GetIntensity());
			this->m_shader_program->SetUniform(root_name + "shadow_far_plane", std::get<1>(point_light->GetClips()));
			this->m_shader_program->SetUniform(root_name + "shadow_bias", point_light->GetShadowBias());

			std::string cubemap_name = "light_shadow_cubemaps[" + std::to_string(i) + "]";

			this->m_shader_program->AddUniformName(cubemap_name);
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
			this->m_shader_program->SetUniform(prefix + "position", obb.GetPosition());
			this->m_shader_program->SetUniform(prefix + "dimensions", obb.GetDimensionsVec());
			this->m_shader_program->SetUniform(prefix + "rotation", glm::mat3(obb.GetRotationMatrix()));
			this->m_shader_program->SetUniform(prefix + "rotation_inverse", glm::mat3(obb.GetRotationMatrixInverse()));
		}

		//previous render result
		if (this->GetTargetType() == GL_TEXTURE_CUBE_MAP)
		{
			this->m_shader_program->SetUniform("render_output_valid", false);
		}
		else
		{
			this->m_shader_program->SetUniform("render_output_valid", this->FramebufferContainsRenderOutput());
		}

		this->m_shader_program->SetUniform("render_output_x", std::get<0>(this->GetOutputSize()));
		this->m_shader_program->SetUniform("render_output_y", std::get<1>(this->GetOutputSize()));

		//load textures from the previous frame (if in normal rendering mode)
		LoadedTexture texture;
		texture.type = std::get<RenderTargetConfig::Normal>(this->m_config.mode_data).previous_frame.type;
		texture.id = std::get<RenderTargetConfig::Normal>(this->m_config.mode_data).previous_frame.colour;
		texture.uniform_name = "render_output_colour";
		this->m_shader_program->SetTexture(-1, texture);

		texture.id = std::get<RenderTargetConfig::Normal>(this->m_config.mode_data).previous_frame.depth;
		texture.uniform_name = "render_output_depth";
		this->m_shader_program->SetTexture(-1, texture);

		for (int i = 0; i < static_cast<int>(std::get<RenderTargetConfig::Normal>(this->m_config.mode_data).previous_frame.data.size()); i++)
		{
			texture.id = std::get<RenderTargetConfig::Normal>(this->m_config.mode_data).previous_frame.data.at(i);
			texture.uniform_name = "render_output_data[" + std::to_string(i) + "]";
			this->m_shader_program->SetTexture(-1, texture);
		}
	}

	if (this->GetRenderMode() == RenderTargetMode::Wireframe)
	{
		this->m_shader_program->SetUniform("draw_back_faces", std::get<RenderTargetConfig::Wireframe>(this->m_config.mode_data).draw_back_faces);
	}
}

void RenderTarget::Render_Setup_FlatQuad()
{
	bool recompile_required = false;

	if (this->GetRenderMode() == RenderTargetMode::Postprocess)
	{
		size_t num_layers = std::get<RenderTargetConfig::PostProcess>(this->m_config.mode_data).layers.size();
		recompile_required = this->m_shader_program->SetDefine("COMPOSITE_LAYER_NUM", std::to_string(num_layers)) ? true : recompile_required;
	}

	if (recompile_required)
	{
		this->m_shader_program->Recompile();
	}

	if (this->GetRenderMode() == RenderTargetMode::Postprocess)
	{
		for (size_t i = 0; i < std::get<RenderTargetConfig::PostProcess>(this->m_config.mode_data).layers.size(); i++)
		{
			const RenderTargetConfig::PostProcess::CompositeLayer& layer = std::get<RenderTargetConfig::PostProcess>(this->m_config.mode_data).layers.at(i);

			LoadedTexture texture;
			texture.id = layer.id;
			texture.type = GL_TEXTURE_2D;
			texture.uniform_name = "layers_texture[" + std::to_string(i) + "]";

			this->m_shader_program->SetTexture(-1, texture);

			const std::string prefix = "layers[" + std::to_string(i) + "].";
			this->m_shader_program->SetUniform(prefix + "colour_translate", layer.colour_translate);
			this->m_shader_program->SetUniform(prefix + "colour_scale", layer.colour_scale);
		}
	}
}

void RenderTarget::Render_ForEachModel_Model(Model* model)
{
	this->m_shader_program->SetUniform("mdl_translate", glm::vec4(model->GetPosition(), 0.0f));
	this->m_shader_program->SetUniform("mdl_rotate", model->GetRotationMatrix());
	this->m_shader_program->SetUniform("mdl_scale", model->GetScaleMatrix());

	if ((this->GetRenderMode() == RenderTargetMode::Normal)
		|| (this->GetRenderMode() == RenderTargetMode::Textured))
	{
		LoadedTexture texture = this->GetEngine()->GetTexture(model->GetColourTexture().GetReference());
		texture.uniform_name = "colourTexture";
		this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), texture);
	}

	if (this->GetRenderMode() == RenderTargetMode::Normal)
	{
		Material& material = model->GetMaterial();
		//material
		this->m_shader_program->SetUniform("mat_diffuse", material.diffuse);
		this->m_shader_program->SetUniform("mat_specular", material.specular);
		this->m_shader_program->SetUniform("mat_specular_highlight", material.specular_highlight);
		this->m_shader_program->SetUniform("mat_displacement_multiplier", material.displacement.multiplier);
		this->m_shader_program->SetUniform("mat_displacement_discard_out_of_range", material.displacement.discard_out_of_range);

		//screen space reflections
		this->m_shader_program->SetUniform("mat_ssr_enabled", material.ssr_enabled);
		this->m_shader_program->SetUniform("mat_ssr_resolution", material.ssr.resolution);
		this->m_shader_program->SetUniform("mat_ssr_max_distance", material.ssr.max_cam_distance);
		this->m_shader_program->SetUniform("mat_ssr_max_cast_distance", material.ssr.cast_distance_limit);
		this->m_shader_program->SetUniform("mat_ssr_depth_acceptance", material.ssr.depth_acceptance);
		this->m_shader_program->SetUniform("mat_ssr_show_this", material.ssr.appear_in_ssr);
		this->m_shader_program->SetUniform("mat_ssr_refinements", material.ssr.refinements);

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
		if (this->GetRenderMode() == RenderTargetMode::Normal && !std::get<RenderTargetConfig::Normal>(this->m_config.mode_data).draw_reflections)
		{
			this->m_shader_program->SetUniform("reflections_enabled", false);
		}
		else
		{
			this->m_shader_program->SetUniform("reflections_enabled", material.reflections_enabled);
		}

		std::vector<std::tuple<Reflection*, ReflectionMode>> reflections = material.reflections;
		for (int i = 0; i < static_cast<int>(reflections.size()); i++)
		{
			Reflection* reflection = std::get<0>(reflections.at(i));
			ReflectionMode reflection_mode = std::get<1>(reflections.at(i));

			std::string prefix = "reflections[" + std::to_string(i) + "].";

			this->m_shader_program->SetUniform(prefix + "position", reflection->GetPosition());
			this->m_shader_program->SetUniform(prefix + "clip_near", std::get<0>(reflection->GetClips()));
			this->m_shader_program->SetUniform(prefix + "clip_far", std::get<1>(reflection->GetClips()));
			this->m_shader_program->SetUniform(prefix + "mode", static_cast<int>(reflection_mode));
			this->m_shader_program->SetUniform(prefix + "iterations", reflection->GetIterations());

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

		this->m_shader_program->SetUniform("reflection_count", static_cast<int>(reflections.size()));

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

	if (this->GetRenderMode() == RenderTargetMode::Wireframe)
	{
		this->m_shader_program->SetUniform("wireframe_colour", model->GetCurrentWireframeColour());
	}

	this->m_shader_program->Select(static_cast<int>(model->GetReference())); //select shader (and texture group)
}

void RenderTarget::Render_ForEachModel_Quad(Model* model)
{
	this->m_shader_program->Select(-1); //select shader (and texture group)
}

RenderTarget::RenderTarget(Engine* engine, RenderTargetConfig config) : m_engine(engine)
{
	this->m_engine->MakeContextCurrent(true); //this is necessary when constructing the first EngineCanvas - call it every time as construction is infrequent and already expensive

	this->m_render_function = [this](std::vector<Model*> model_pool)
	{
		this->RenderScene(model_pool);
	};

	this->m_shader_program = std::make_unique<ShaderProgram>();
	this->SetConfig(config);
}

RenderTarget::~RenderTarget()
{
}

void RenderTarget::SetCamera(Camera* camera)
{
	this->m_camera = camera;
}

Camera* RenderTarget::GetCamera() const
{
	return this->m_camera;
}

Engine* RenderTarget::GetEngine() const
{
	return this->m_engine;
}

void RenderTarget::Render(std::vector<Model*> models, bool continuous_draw)
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

void RenderTarget::SetRenderFunction(ControllerFunction function)
{
	this->m_render_function = function;
}

std::unordered_set<RenderTextureReference> RenderTarget::GetRenderTextureDependencies() const
{
	std::unordered_set<RenderTextureReference> result;

	if (this->GetRenderMode() == RenderTargetMode::Normal)
	{
		for (std::tuple<Cubemap*, CubemapType> cubemap_data : this->GetEngine()->GetScene()->GetCubemaps())
		{
			Cubemap* cubemap = std::get<0>(cubemap_data);
			CubemapType cubemap_type = std::get<1>(cubemap_data);

			if (!(cubemap_type == CubemapType::Reflection && !std::get<RenderTargetConfig::Normal>(this->m_config.mode_data).draw_reflections))
			{
				result.insert(cubemap->GetReference());
			}
		}
	}

	return result;
}

void RenderTarget::CopyFrom(const RenderTarget* src) const
{
	if (this != src)
	{
		const RenderTexture* this_tex = dynamic_cast<const RenderTexture*>(this);
		const RenderTexture* src_tex = dynamic_cast<const RenderTexture*>(src);

		if ((this_tex != nullptr) && (src_tex != nullptr))
		{
			CopyTextureGroup(src_tex->GetOutputTextures(), this_tex->GetWriteTextures(), this_tex->GetTextureInfo(), this_tex->GetOutputSize());
		}
	}
	/*if ((this != src) && (this->GetFramebuffer() != src->GetFramebuffer()))
	{
		auto [src_x, src_y] = src->GetOutputSize();
		auto [dest_x, dest_y] = this->GetOutputSize();

		GLbitfield mask = GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT;

		glBindFramebuffer(GL_READ_FRAMEBUFFER, src->GetFramebuffer());
		glBindFramebuffer(GL_DRAW_FRAMEBUFFER, this->GetFramebuffer());

		glBlitFramebuffer(
			0, 0, static_cast<GLint>(src_x), static_cast<GLint>(src_y),
			0, 0, static_cast<GLint>(dest_x), static_cast<GLint>(dest_y),
			mask, GL_NEAREST
		);
	}*/
}

void RenderTarget::CopyTo(const RenderTarget* dest) const
{
	dest->CopyFrom(this);
}

RenderTargetMode RenderTarget::GetRenderMode() const
{
	return this->m_config.mode;
}

void RenderTarget::SetConfig(RenderTargetConfig config)
{
	if (this->m_config.mode != config.mode)
	{
		if (config.mode == RenderTargetMode::Postprocess)
		{
			this->m_postprocess_model = std::make_unique<Model>(-1, std::vector<std::shared_ptr<Geometry>>({ std::make_shared<PresetGeometry>(PresetGeometry::GeometryType::Plane) }));
		}
		else
		{
			this->m_postprocess_model.reset();
		}
	}

	this->m_config = config;
	this->m_fbo_contains_render = false;

	std::vector<ShaderProgram::ShaderSource> shaders;
	if (this->RenderModeIsModelRendering(this->GetRenderMode()))
	{
		shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_VERTSHADER), GL_VERTEX_SHADER));
		shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_TESSCONTROLSHADER), GL_TESS_CONTROL_SHADER));
		shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_TESSEVALSHADER), GL_TESS_EVALUATION_SHADER));

		if (this->GetRenderMode() == RenderTargetMode::Normal)
		{
			shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_GEOMSHADER), GL_GEOMETRY_SHADER));
			shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_NORMAL_PASS0_FRAGSHADER), GL_FRAGMENT_SHADER));
		}
		else if (this->GetRenderMode() == RenderTargetMode::Wireframe)
		{
			shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_WIREFRAME_GEOMSHADER), GL_GEOMETRY_SHADER));
			shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_WIREFRAME_FRAGSHADER), GL_FRAGMENT_SHADER));
		}
		else if (this->GetRenderMode() == RenderTargetMode::Shadow)
		{
			shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_GEOMSHADER), GL_GEOMETRY_SHADER));
			shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_SHADOW_FRAGSHADER), GL_FRAGMENT_SHADER));
		}
		else if (this->GetRenderMode() == RenderTargetMode::Textured)
		{
			shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_GEOMSHADER), GL_GEOMETRY_SHADER));
			shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_TEXTURED_FRAGSHADER), GL_FRAGMENT_SHADER));
		}
	}
	else if (this->GetRenderMode() == RenderTargetMode::Postprocess)
	{
		shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_POSTPROCESS_FRAGSHADER), GL_FRAGMENT_SHADER));
		shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_POSTPROCESS_VERTSHADER), GL_VERTEX_SHADER));
	}
	else if (this->GetRenderMode() == RenderTargetMode::Default)
	{
		shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_DEFAULT_FRAGSHADER), GL_FRAGMENT_SHADER));
		shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_DEFAULT_VERTSHADER), GL_VERTEX_SHADER));
	}

	this->m_shader_program->SetShaderSources(shaders);
	this->m_shader_program->Recompile();

	if (this->RenderModeIsModelRendering(this->GetRenderMode()))
	{
		this->m_shader_program->AddUniformNames({
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
			"tess_interp_mode",
			"patch_size_u",
			"patch_size_v",
			//geometry
			"cubemap_transform[0]",
			"cubemap_transform[1]",
			"cubemap_transform[2]",
			"cubemap_transform[3]",
			"cubemap_transform[4]",
			"cubemap_transform[5]",
			"is_cubemap"
			});

		if (this->GetRenderMode() == RenderTargetMode::Normal)
		{
			this->m_shader_program->AddUniformNames({
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
				this->m_shader_program->AddUniformName("render_output_data[" + std::to_string(i) + "]");
			}
		}
		else if (this->GetRenderMode() == RenderTargetMode::Wireframe)
		{
			this->m_shader_program->AddUniformNames({
				//geometry
				"draw_back_faces",
				//fragment
				"wireframe_colour"
				});
		}
		else if (this->GetRenderMode() == RenderTargetMode::Textured)
		{
			this->m_shader_program->AddUniformNames({
				//fragment
				"colourTexture"
				});
		}
	}
}

void RenderTarget::SetModeConfig(RenderTargetConfig::Normal mode_config)
{
	RenderTargetConfig config = this->m_config;
	config.mode = RenderTargetMode::Normal;
	config.mode_data = mode_config;
	this->SetConfig(config);
}

void RenderTarget::SetModeConfig(RenderTargetConfig::Wireframe mode_config)
{
	RenderTargetConfig config = this->m_config;
	config.mode = RenderTargetMode::Wireframe;
	config.mode_data = mode_config;
	this->SetConfig(config);
}

void RenderTarget::SetModeConfig(RenderTargetConfig::Shadow mode_config)
{
	RenderTargetConfig config = this->m_config;
	config.mode = RenderTargetMode::Shadow;
	config.mode_data = mode_config;
	this->SetConfig(config);
}

void RenderTarget::SetModeConfig(RenderTargetConfig::PostProcess mode_config)
{
	RenderTargetConfig config = this->m_config;
	config.mode = RenderTargetMode::Postprocess;
	config.mode_data = mode_config;
	this->SetConfig(config);
}

void RenderTarget::SetModeConfig(RenderTargetConfig::Textured mode_config)
{
	RenderTargetConfig config = this->m_config;
	config.mode = RenderTargetMode::Textured;
	config.mode_data = mode_config;
	this->SetConfig(config);
}

RenderTargetConfig RenderTarget::GetConfig() const
{
	return this->m_config;
}
