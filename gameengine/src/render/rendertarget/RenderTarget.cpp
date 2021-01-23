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
#include "../NormalModeConstants.h"

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
			models.push_back(this->m_fsquad_model.get());
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

		if (this->RenderModeIsModelRendering())
		{
			switch (this->GetRenderMode())
			{
			case RenderTargetMode::Normal_FirstPass:
				glEnable(GL_CULL_FACE);
				glCullFace(GL_BACK);

				glEnable(GL_DEPTH_TEST);
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
			default:
				throw std::runtime_error("Unknown render target mode");
			}
		}
		else
		{
			glDisable(GL_CULL_FACE);

			glDisable(GL_DEPTH_TEST);
		}

		//prepare viewport
		glViewport(0, 0, std::get<0>(this->GetOutputSize()), std::get<1>(this->GetOutputSize()));

		glm::vec4 clear_colour;
		if (this->GetRenderMode() == RenderTargetMode::Normal_FirstPass)
		{
			clear_colour = glm::vec4(0.0f);
		}
		else
		{
			clear_colour = this->m_engine->GetScene()->GetClearColour();
		}

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
			else
			{
				if (info.primitive_type == Geometry::PrimitiveType::Triangles)
				{
					mode = GL_TRIANGLES;
				}
				else
				{
					throw std::runtime_error("The only primitive type supported by full screen quad rendering is triangles");
				}
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
	return (mode == RenderTargetMode::Normal_FirstPass)
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

	if ((this->GetRenderMode() == RenderTargetMode::Wireframe)
		|| (this->GetRenderMode() == RenderTargetMode::Textured))
	{
		//data textures
		recompile_required = this->m_shader_program->SetDefine("DATA_TEX_NUM", std::to_string(GAMEENGINE_NUM_DATA_TEX)) ? true : recompile_required;
	}
	else if (this->GetRenderMode() == RenderTargetMode::Normal_FirstPass)
	{
		recompile_required = this->m_shader_program->SetDefine("PASSTHROUGH_TEX_NUM", std::to_string(GAMEENGINE_NORMAL_PASSTHROUGH_TEX)) ? true : recompile_required;
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
	else if (this->GetRenderMode() == RenderTargetMode::Normal_LastPass)
	{
		recompile_required = this->m_shader_program->SetDefine("DATA_TEX_NUM", std::to_string(GAMEENGINE_NUM_DATA_TEX)) ? true : recompile_required;
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
	else if (this->GetRenderMode() == RenderTargetMode::Normal_LastPass)
	{
		LoadedTexture texture;

		texture.id = std::get<RenderTargetConfig::Normal_LastPass>(this->m_config.mode_data).first_pass.colour;
		texture.type = std::get<RenderTargetConfig::Normal_LastPass>(this->m_config.mode_data).first_pass.type;
		texture.uniform_name = "gbufferTextureSample";
		this->m_shader_program->SetTexture(-1, texture);
	}
}

void RenderTarget::Render_ForEachModel_Model(Model* model)
{
	this->m_shader_program->SetUniform("mdl_translate", glm::vec4(model->GetPosition(), 0.0f));
	this->m_shader_program->SetUniform("mdl_rotate", model->GetRotationMatrix());
	this->m_shader_program->SetUniform("mdl_scale", model->GetScaleMatrix());

	if ((this->GetRenderMode() == RenderTargetMode::Normal_FirstPass)
		|| (this->GetRenderMode() == RenderTargetMode::Textured))
	{
		LoadedTexture texture = this->GetEngine()->GetTexture(model->GetColourTexture().GetReference());
		texture.uniform_name = "colourTexture";
		this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), texture);
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

	/*if (this->GetRenderMode() == RenderTargetMode::Normal_LastPass)
	{
		for (std::tuple<Cubemap*, CubemapType> cubemap_data : this->GetEngine()->GetScene()->GetCubemaps())
		{
			Cubemap* cubemap = std::get<0>(cubemap_data);
			CubemapType cubemap_type = std::get<1>(cubemap_data);

			if (!(cubemap_type == CubemapType::Reflection && !std::get<RenderTargetConfig::Normal_LastPass>(this->m_config.mode_data).draw_reflections))
			{
				result.insert(cubemap->GetReference());
			}
		}
	}*/

	return result;
}

void RenderTarget::CopyFrom(const RenderTarget* src) const
{
	if ((this != src) && (this->GetFramebuffer() != src->GetFramebuffer()))
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
	}
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
		if (this->RenderModeIsModelRendering())
		{
			this->m_fsquad_model.reset();
		}
		else
		{
			this->m_fsquad_model = std::make_unique<Model>(-1, std::vector<std::shared_ptr<Geometry>>({ std::make_shared<PresetGeometry>(PresetGeometry::GeometryType::Plane) }));
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

		if (this->GetRenderMode() == RenderTargetMode::Normal_FirstPass)
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
	else if (this->GetRenderMode() == RenderTargetMode::Normal_LastPass)
	{
		shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_NORMAL_PASS1_FRAGSHADER), GL_FRAGMENT_SHADER));
		shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_NORMAL_PASS1_VERTSHADER), GL_VERTEX_SHADER));
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

		if (this->GetRenderMode() == RenderTargetMode::Normal_FirstPass)
		{
			this->m_shader_program->AddUniformNames({
				//fragment
				"colourTexture"
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
	else if (this->GetRenderMode() == RenderTargetMode::Normal_LastPass)
	{
		this->m_shader_program->AddUniformNames({
			//fragment
			"gbufferTextureSample"
			});
	}
}

void RenderTarget::SetModeConfig(RenderTargetConfig::Normal_FirstPass mode_config)
{
	RenderTargetConfig config = this->m_config;
	config.mode = RenderTargetMode::Normal_FirstPass;
	config.mode_data = mode_config;
	this->SetConfig(config);
}

void RenderTarget::SetModeConfig(RenderTargetConfig::Normal_LastPass mode_config)
{
	RenderTargetConfig config = this->m_config;
	config.mode = RenderTargetMode::Normal_LastPass;
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
