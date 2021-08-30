#include "RenderTarget.h"

#include <stdexcept>
#include <string>

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include "../../ShaderProgram.h"
#include "../../../Engine.h"
#include "../../../Resource.h"
#include "../../../scene/Camera.h"
#include "../../../scene/Scene.h"
#include "../../../scene/OrientedBoundingBox.h"
#include "../../../scene/Skybox.h"
#include "../../../scene/light/PointLight.h"
#include "../../../scene/model/Model.h"
#include "../../../scene/model/geometry/PresetGeometry.h"
#include "../../../scene/model/geometry/Patch.h"
#include "../../TargetType.h"
#include "../../../LogMessage.h"

#include "../../gltexture/GLTextureDataPreset.h"
#include "../../glgeometry/GLGeometry.h"

#include "../texture/RenderTexture.h"
#include "../texture/RenderTextureGroup.h"

void RenderTarget::RenderScene(std::vector<Model*> models, std::clock_t draw_time)
{
	if (this->GetRenderMode() == RenderTargetMode::Default)
	{
		throw std::runtime_error("Can't render when in default mode");
	}

	if (this->m_engine->GetScene() != nullptr)
	{
#ifdef _DEBUG
		if (!this->m_fbo.IsValidFBO())
		{
			throw std::runtime_error("FBO provided is not an FBO");
		}
#endif

		this->m_fbo.Bind();

#ifdef _DEBUG
		{
			GLenum fbo_status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
			if (fbo_status != GL_FRAMEBUFFER_COMPLETE)
			{
				std::string fbo_status_info = "";
				switch (fbo_status) //https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCheckFramebufferStatus.xhtml
				{
				case GL_FRAMEBUFFER_UNDEFINED: fbo_status_info = "GL_FRAMEBUFFER_UNDEFINED"; break;
				case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT: fbo_status_info = "GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT"; break;
				case GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT: fbo_status_info = "GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT"; break;
				case GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER: fbo_status_info = "GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER"; break;
				case GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER: fbo_status_info = "GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER"; break;
				case GL_FRAMEBUFFER_UNSUPPORTED: fbo_status_info = "GL_FRAMEBUFFER_UNSUPPORTED"; break;
				case GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE: fbo_status_info = "GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE"; break;
				case GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS: fbo_status_info = "GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS"; break;
				default: fbo_status_info = "unknown";
				}

				throw std::runtime_error("Invalid framebuffer status: " + fbo_status_info);
			}
		}
#endif

		if (this->RenderModeIsModelRendering())
		{
			models = this->Render_GetModels_Model(models);
		}

		this->m_shader_program->Select();

		this->m_shader_program->SetDefine("TARGET_IS_CUBEMAP", static_cast<int>(this->GetTargetType() == TargetType::Texture_Cubemap));

		if (this->RenderModeIsModelRendering())
		{
			this->Render_Setup_Model(models);
		}
		else
		{
			this->Render_Setup_FSQuad();
		}

		const auto& [output_size_x, output_size_y] = this->GetOutputSize();
		glm::ivec2 output_size = glm::ivec2(output_size_x, output_size_y);

		//global uniforms
		this->m_shader_program->SetUniform("is_cubemap", this->GetTargetType() == TargetType::Texture_Cubemap);
		this->m_shader_program->SetUniform("render_output_dimensions", output_size);

		switch (this->GetRenderMode())
		{
		case RenderTargetMode::Normal_DepthOnly:
		case RenderTargetMode::Normal_Draw:
			glEnable(GL_CULL_FACE);
			glCullFace(GL_BACK);

			glEnable(GL_DEPTH_TEST);
			break;
		case RenderTargetMode::PostProcess:
		case RenderTargetMode::Normal_PostProcess:
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

		if (this->GetRenderMode() == RenderTargetMode::Normal_Draw)
		{
			glDepthFunc(GL_LEQUAL);
		}
		else
		{
			glDepthFunc(GL_LESS);
		}

		//prepare viewport
		glViewport(0, 0, output_size.x, output_size.y);

		this->CheckParentContext();

		//draw scene geometry
		if (this->RenderModeIsModelRendering())
		{
			bool has_cleared = false;

			for (Model* model : models)
			{
				this->Render_ForEachModel_Model(model, draw_time);

				if (!has_cleared)
				{
					this->DoClear();
					has_cleared = true;
				}

				for (const std::shared_ptr<Geometry>& geometry : model->GetGeometry())
				{
					Geometry::RenderInfo render_info = geometry->GetRenderInfo();
					if ((render_info.primitive_type == Geometry::PrimitiveType::Patches) || (render_info.primitive_type == Geometry::PrimitiveType::Quads))
					{
						this->m_shader_program->SetUniform("tess_enable", render_info.tesselation_enabled);
						this->m_shader_program->SetUniform("tess_interp_mode", static_cast<int>(render_info.interpolation_mode));

						this->m_shader_program->SetUniform("patch_size_u", render_info.primitive_dimensions.x);
						this->m_shader_program->SetUniform("patch_size_v", render_info.primitive_dimensions.y);
					}
					else
					{
						throw std::runtime_error("Primitive type " + GetPrimitiveTypeName(render_info.primitive_type) + " is not supported by model-oriented rendering modes");
					}

#ifdef _DEBUG
					this->CheckShaderValidity();
#endif

					this->GetEngine()->GetGeometry(geometry)->Draw();
				}

				this->CheckParentContext();
			}

			if (!has_cleared)
			{
				this->m_shader_program->Select();
				this->DoClear();
				has_cleared = true;
			}
		}
		else
		{
			this->DoClear();
			this->Render_ForEachModel_FSQuad();

#ifdef _DEBUG
			this->CheckShaderValidity();
#endif

			this->GetEngine()->GetGeometry(PresetGeometry(PresetGeometry::GeometryType::Plane))->Draw();
			this->CheckParentContext();
		}

		this->m_fbo_contains_render = true;

		glBindVertexArray(NULL);
		glBindBuffer(GL_ARRAY_BUFFER, NULL);
	}
}

void RenderTarget::CheckParentContext() const
{
#ifdef _DEBUG
	if (!this->GetEngine()->ContextIsValid())
	{
		throw std::runtime_error("Context of the Engine that this RenderTarget is a child of is no longer valid");
	}
#endif
}

void RenderTarget::SetFramebuffer(GLFramebuffer&& framebuffer)
{
	this->m_fbo = std::move(framebuffer);
	this->m_fbo_contains_render = false;
}

const GLFramebuffer& RenderTarget::GetFramebuffer() const
{
	return this->m_fbo;
}

GLFramebuffer& RenderTarget::GetFramebuffer()
{
	return this->m_fbo;
}

void RenderTarget::SetTargetType(TargetType target_type)
{
	this->m_fbo_target = target_type;
}

TargetType RenderTarget::GetTargetType() const
{
	return this->m_fbo_target;
}

bool RenderTarget::FramebufferContainsRenderOutput() const
{
	return this->m_fbo_contains_render;
}

bool RenderTarget::IsFBOClearedOnRender() const
{
	return this->m_config.clear_fbo;
}

void RenderTarget::PreRenderEvent()
{
}

void RenderTarget::PostRenderEvent()
{
}

bool RenderTarget::RenderModeIsModelRendering() const
{
	return GetRenderTargetModeType(this->GetRenderMode()) == RenderTargetModeType::Model;
}

bool RenderTarget::RenderModeIsFSQuadRendering() const
{
	return GetRenderTargetModeType(this->GetRenderMode()) == RenderTargetModeType::FSQuad;
}

std::vector<Model*> RenderTarget::Render_GetModels_Model(std::vector<Model*> model_pool)
{
	return this->GetEngine()->GetScene()->GetVisibleModels(this->m_camera->GetPosition(), this->GetRenderMode(), model_pool);
}

void RenderTarget::Render_Setup_Model(std::vector<Model*> models)
{
	if (this->GetRenderMode() == RenderTargetMode::Normal_Draw)
	{
		//point lights
		this->m_shader_program->SetDefine("POINT_LIGHT_NUM", static_cast<int>(this->GetEngine()->GetScene()->GetPointLights().size()));

		//OBB approximations
		this->m_shader_program->SetDefine("APPROXIMATION_OBB_NUM", static_cast<int>(this->GetEngine()->GetScene()->GetOBBApproximations().size()));

		//reflections
		this->m_shader_program->SetDefine("REFLECTION_NUM", static_cast<int>(this->GetEngine()->GetScene()->GetReflections().size()));

		this->m_shader_program->SetDefine("NUM_NORMAL_DEPTHONLY_TEXTURES", GetNumAttachedColourTextures(RenderTargetMode::Normal_DepthOnly));
		this->m_shader_program->SetDefine("NUM_NORMAL_TEXTURES", GetNumAttachedColourTextures(RenderTargetMode::Normal_PostProcess));
	}

	if (this->GetRenderMode() == RenderTargetMode::Normal_DepthOnly || this->GetRenderMode() == RenderTargetMode::Normal_Draw)
	{
		//determine if any models might need to discard fragments
		bool frags_may_be_discarded = false;

		for (Model* model : models)
		{
			if (model->GetMaterial().displacement.discard_out_of_range)
			{
				frags_may_be_discarded = true;
			}
		}

		this->m_shader_program->SetDefine("SUPPORT_DISPLACEMENT_OUT_OF_RANGE_DISCARDING", frags_may_be_discarded ? 1 : 0);
	}

	this->m_shader_program->Recompile();

	//load "constant" uniforms (uniforms constant between models like camera data) into program
	// cubemaps
	if (this->GetTargetType() == TargetType::Texture_Cubemap)
	{
		this->m_shader_program->SetUniform("cubemap_transform", this->m_cubemap_rotations.begin(), this->m_cubemap_rotations.end());
	}
	else
	{
		this->m_shader_program->SetUniform("cubemap_transform[0]", glm::mat4(1.0f));
		this->m_shader_program->SetUniform("cubemap_transform[1]", glm::mat4(1.0f));
		this->m_shader_program->SetUniform("cubemap_transform[2]", glm::mat4(1.0f));
		this->m_shader_program->SetUniform("cubemap_transform[3]", glm::mat4(1.0f));
		this->m_shader_program->SetUniform("cubemap_transform[4]", glm::mat4(1.0f));
		this->m_shader_program->SetUniform("cubemap_transform[5]", glm::mat4(1.0f));
	}

	// camera
	this->m_shader_program->SetUniform("cam_translate", glm::vec4(0.0 - this->GetCamera()->GetPosition(), 0.0f));
	this->m_shader_program->SetUniform("cam_rotate", this->GetCamera()->GetRotationMatrixInverse());
	this->m_shader_program->SetUniform("cam_persp", this->GetCamera()->GetPerspectiveMatrix());
	this->m_shader_program->SetUniform("cam_clip_near", std::get<0>(this->GetCamera()->GetClips()));
	this->m_shader_program->SetUniform("cam_clip_far", std::get<1>(this->GetCamera()->GetClips()));
	this->m_shader_program->SetUniform("cam_transform", this->GetCamera()->GetCombinedMatrix());
	this->m_shader_program->SetUniform("cam_transform_inverse", glm::inverse(this->GetCamera()->GetCombinedMatrix()));

	if (this->GetRenderMode() == RenderTargetMode::Normal_Draw)
	{
		// ambient light
		this->m_shader_program->SetUniform("light_ambient", this->GetEngine()->GetScene()->GetAmbientLight());

		// shadows
		bool shadows_enabled = true;
		if (!this->m_config.Data<RenderTargetConfig::Normal_Draw>().draw_shadows)
		{
			shadows_enabled = false;
		}
		this->m_shader_program->SetUniform("light_shadow_draw", shadows_enabled);

		//point lights
		const std::vector<std::shared_ptr<PointLight>>& point_lights = this->GetEngine()->GetScene()->GetPointLights();
		for (int i = 0; i < static_cast<int>(point_lights.size()); i++)
		{
			const std::shared_ptr<PointLight>& point_light = point_lights.at(i);
			std::string root_name = "light_points[" + std::to_string(i) + "].";
			this->m_shader_program->SetUniform(root_name + "position", point_light->GetPosition());
			this->m_shader_program->SetUniform(root_name + "intensity", point_light->GetIntensity());
			this->m_shader_program->SetUniform(root_name + "shadow_far_plane", std::get<1>(point_light->GetClips()));
			this->m_shader_program->SetUniform(root_name + "shadow_bias", point_light->GetShadowBias());
			this->m_shader_program->SetUniform(root_name + "texture_dimensions", std::get<0>(point_light->GetTextureDimensions()));

			std::string cubemap_name = "light_shadow_cubemaps[" + std::to_string(i) + "]";

			this->m_shader_program->AddUniformName(cubemap_name);
			std::shared_ptr<RenderTextureGroup> texture = this->GetEngine()->GetRenderTexture(point_light->GetReference());

			this->m_shader_program->SetTexture(-1, cubemap_name, texture->depth.value());
		}

		//scene approximation
		const std::vector<OrientedBoundingBox>& scene_approximations = this->GetEngine()->GetScene()->GetOBBApproximations();
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
		bool render_output_valid = false;
		if (this->GetTargetType() == TargetType::Texture_2D)
		{
			render_output_valid = this->FramebufferContainsRenderOutput();
		}

		this->m_shader_program->SetUniform("render_output_valid", render_output_valid);

		if (render_output_valid)
		{
			//load textures from the previous frame (if in normal rendering mode)
			std::shared_ptr<RenderTextureGroup> depth_frame = this->m_config.Data<RenderTargetConfig::Normal_Draw>().depth_frame;

			for (int i = 0; i < GetNumColourTextures(RenderTargetMode::Normal_DepthOnly).value(); i++)
			{
				this->m_shader_program->SetTexture(-1, "render_output_colour[" + std::to_string(i) + "]", depth_frame->colour.at(i));
			}

			this->m_shader_program->SetTexture(-1, "render_output_depth", depth_frame->depth.value());
		}
		else
		{
			for (int i = 0; i < GetNumColourTextures(RenderTargetMode::Normal_DepthOnly).value(); i++)
			{
				this->m_shader_program->SetTexture(-1, "render_output_colour[" + std::to_string(i) + "]", this->GetEngine()->GetTexture(GLTextureDataPreset::Black, TargetType::Texture_2D));
			}

			this->m_shader_program->SetTexture(-1, "render_output_depth", this->GetEngine()->GetTexture(GLTextureDataPreset::ZeroShadow, TargetType::Texture_2D));
		}

		//ssr quality
		std::shared_ptr<RenderTextureGroup> ssr_quality_frame = this->m_config.Data<RenderTargetConfig::Normal_Draw>().ssr_quality_frame;
		this->m_shader_program->SetTexture(-1, "render_ssr_quality", ssr_quality_frame->colour.at(0));
	}

	if (this->GetRenderMode() == RenderTargetMode::Wireframe)
	{
		this->m_shader_program->SetUniform("draw_back_faces", this->m_config.Data<RenderTargetConfig::Wireframe>().draw_back_faces);
	}
}

void RenderTarget::Render_Setup_FSQuad()
{
	using RTCPostProcess = RenderTargetConfig::PostProcess;

	if (this->GetRenderMode() == RenderTargetMode::PostProcess)
	{
		size_t num_layers = this->m_config.Data<RTCPostProcess>().layers.size();
		this->m_shader_program->SetDefine("LAYER_NUM", static_cast<int>(num_layers));
	}
	else if (this->GetRenderMode() == RenderTargetMode::Normal_PostProcess || this->GetRenderMode() == RenderTargetMode::Normal_SSRQuality)
	{
		this->m_shader_program->SetDefine("NUM_NORMAL_DRAW_TEXTURES", GetNumAttachedColourTextures(RenderTargetMode::Normal_Draw));
	}

	this->m_shader_program->Recompile();

	if (this->GetRenderMode() == RenderTargetMode::PostProcess)
	{
		for (size_t i = 0; i < this->m_config.Data<RTCPostProcess>().layers.size(); i++)
		{
			const RTCPostProcess::Layer& layer = this->m_config.Data<RTCPostProcess>().layers.at(i);

			this->m_shader_program->SetTexture(-1, "layers_texture[" + std::to_string(i) + "]", layer.texture);

			const std::string prefix = "layers[" + std::to_string(i) + "].";
			this->m_shader_program->SetUniform(prefix + "colour_translate", layer.colour_translate);
			this->m_shader_program->SetUniform(prefix + "colour_scale", layer.colour_scale);
		}

		RTCPostProcess::Mode mode = this->m_config.Data<RTCPostProcess>().GetMode();
		if (mode == RTCPostProcess::Mode::Uninitialised)
		{
			throw std::runtime_error("Given PostProcess config mode has not been initialised");
		}

		this->m_shader_program->SetUniform("mode", static_cast<int>(mode));

		if (mode == RTCPostProcess::Mode::AlphaBlend)
		{

		}
		else if ((mode == RTCPostProcess::Mode::BoxBlur) || (mode == RTCPostProcess::Mode::MaxBox))
		{
			std::string prefix;
			glm::ivec2 radius;
			bool is_first_pass;
			if (mode == RTCPostProcess::Mode::BoxBlur)
			{
				prefix = "modedata_BoxBlur.";

				radius = this->m_config.Data<RTCPostProcess>().Data<RTCPostProcess::BoxBlur>().radius;
				is_first_pass = this->m_config.Data<RTCPostProcess>().Data<RTCPostProcess::BoxBlur>().is_first_pass;
			}
			else if (mode == RTCPostProcess::Mode::MaxBox)
			{
				prefix = "modedata_MaxBox.";

				radius = this->m_config.Data<RTCPostProcess>().Data<RTCPostProcess::MaxBox>().radius;
				is_first_pass = this->m_config.Data<RTCPostProcess>().Data<RTCPostProcess::MaxBox>().is_first_pass;
			}

			this->m_shader_program->SetUniform(prefix + "radius", radius);
			this->m_shader_program->SetUniform(prefix + "is_first_pass", is_first_pass);

			if (this->m_config.Data<RTCPostProcess>().layers.size() < 1)
			{
				throw std::runtime_error("Must provide at least 1 layer");
			}
		}
		else
		{
			throw std::runtime_error("Unknown post process mode");
		}
	}
	else if (this->GetRenderMode() == RenderTargetMode::Normal_PostProcess)
	{
		std::shared_ptr<RenderTextureGroup>& draw_frame = this->m_config.Data<RenderTargetConfig::Normal_PostProcess>().draw_frame;
		for (int i = 0; i < static_cast<int>(draw_frame->colour.size()); i++)
		{
			this->m_shader_program->SetTexture(-1, "draw_frame[" + std::to_string(i) + "]", draw_frame->colour.at(i));
		}

		this->m_shader_program->SetTexture(-1, "draw_frame_depth", draw_frame->depth.value());
	}
	else if (this->GetRenderMode() == RenderTargetMode::Normal_SSRQuality)
	{
		std::shared_ptr<RenderTextureGroup>& draw_frame = this->m_config.Data<RenderTargetConfig::Normal_SSRQuality>().draw_frame;
		for (int i = 0; i < static_cast<int>(draw_frame->colour.size()); i++)
		{
			this->m_shader_program->SetTexture(-1, "draw_frame[" + std::to_string(i) + "]", draw_frame->colour.at(i));
		}

		this->m_shader_program->SetTexture(-1, "draw_frame_depth", draw_frame->depth.value());

		const auto& [draw_size_x, draw_size_y] = draw_frame->GetDimensions();
		this->m_shader_program->SetUniform("render_draw_output_dimensions", glm::ivec2(draw_size_x, draw_size_y));
	}
}

void RenderTarget::Render_ForEachModel_Model(Model* model, std::clock_t draw_time)
{
	this->m_shader_program->SetUniform("mdl_translate", glm::vec4(model->GetPosition() + model->GetPositionMotionDescriptor().GetCurrentDisplacement(draw_time), 0.0f));
	this->m_shader_program->SetUniform("mdl_rotate", GetRotationMatrix(model->GetRotation() + model->GetRotationMotionDescriptor().GetCurrentDisplacement(draw_time)));
	this->m_shader_program->SetUniform("mdl_scale", GetScaleMatrix(model->GetScale() + model->GetScaleMotionDescriptor().GetCurrentDisplacement(draw_time)));

	if ((this->GetRenderMode() == RenderTargetMode::Normal_Draw)
		|| (this->GetRenderMode() == RenderTargetMode::Textured))
	{
		this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), "colourTexture", this->GetEngine()->GetTexture(model->GetColourTexture().GetReference()));
	}

	if (this->GetRenderMode() == RenderTargetMode::Normal_DepthOnly || this->GetRenderMode() == RenderTargetMode::Normal_Draw)
	{
		Material& material = model->GetMaterial();
		this->m_shader_program->SetUniform("mat_displacement_multiplier", material.displacement.multiplier);
		this->m_shader_program->SetUniform("mat_displacement_discard_out_of_range", material.displacement.discard_out_of_range);
		this->m_shader_program->SetUniform("mat_ssr_show_this", material.ssr.appear_in_ssr);

		this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), "displacementTexture", this->GetEngine()->GetTexture(model->GetDisplacementTexture().GetReference()));
	}

	if (this->GetRenderMode() == RenderTargetMode::Normal_Draw)
	{
		Material& material = model->GetMaterial();
		//material
		this->m_shader_program->SetUniform("mat_diffuse", material.diffuse);
		this->m_shader_program->SetUniform("mat_specular", material.specular);
		this->m_shader_program->SetUniform("mat_specular_highlight", material.specular_highlight);

		//screen space reflections
		this->m_shader_program->SetUniform("mat_ssr_enabled", material.ssr_enabled && (this->GetTargetType() == TargetType::Texture_2D));
		this->m_shader_program->SetUniform("mat_ssr_resolution", material.ssr.resolution);
		this->m_shader_program->SetUniform("mat_ssr_max_distance", material.ssr.max_cam_distance);
		this->m_shader_program->SetUniform("mat_ssr_max_cast_distance", material.ssr.cast_distance_limit);
		this->m_shader_program->SetUniform("mat_ssr_depth_acceptance", material.ssr.depth_acceptance);
		this->m_shader_program->SetUniform("mat_ssr_show_this", material.ssr.appear_in_ssr);
		this->m_shader_program->SetUniform("mat_ssr_refinements_min", material.ssr.refinements_min);
		this->m_shader_program->SetUniform("mat_ssr_refinements_max", material.ssr.refinements_max);

		//textures
		this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), "normalTexture", this->GetEngine()->GetTexture(model->GetNormalTexture().GetReference()));
		this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), "specularTexture", this->GetEngine()->GetTexture(model->GetSpecularTexture().GetReference()));
		this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), "reflectionIntensityTexture", this->GetEngine()->GetTexture(model->GetReflectionTexture().GetReference()));
		this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), "skyboxMaskTexture", this->GetEngine()->GetTexture(model->GetSkyboxMaskTexture().GetReference()));

		//reflections
		if (this->GetRenderMode() == RenderTargetMode::Normal_Draw && !this->m_config.Data<RenderTargetConfig::Normal_Draw>().draw_reflections)
		{
			this->m_shader_program->SetUniform("reflections_enabled", false);
		}
		else
		{
			this->m_shader_program->SetUniform("reflections_enabled", material.reflections_enabled);
		}

		int num_colour_tex = GetNumColourTextures(RenderTargetMode::Normal_PostProcess).value();

		const std::vector<std::tuple<std::shared_ptr<Reflection>, ReflectionMode>>& reflections = material.reflections;
		for (int i = 0; i < static_cast<int>(reflections.size()); i++)
		{
			const std::shared_ptr<Reflection>& reflection = std::get<0>(reflections.at(i));
			ReflectionMode reflection_mode = std::get<1>(reflections.at(i));

			std::string prefix = "reflections[" + std::to_string(i) + "].";

			this->m_shader_program->SetUniform(prefix + "position", reflection->GetPosition());
			this->m_shader_program->SetUniform(prefix + "clip_near", std::get<0>(reflection->GetClips()));
			this->m_shader_program->SetUniform(prefix + "clip_far", std::get<1>(reflection->GetClips()));
			this->m_shader_program->SetUniform(prefix + "mode", static_cast<int>(reflection_mode));

			std::shared_ptr<RenderTextureGroup> reflection_output = this->GetEngine()->GetRenderTexture(reflection->GetReference());
			for (int j = 0; j < num_colour_tex; j++)
			{
				std::string uniform_name = "reflection_cubemaps[" + std::to_string((i * num_colour_tex) + j) + "]";
				this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), uniform_name, reflection_output->colour.at(j));
			}
			this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), "reflection_depth_cubemaps[" + std::to_string(i) + "]", reflection_output->depth.value());
		}

		int required_reflections = this->m_shader_program->GetDefine<int>("REFLECTION_NUM");
		for (int i = static_cast<int>(reflections.size()); i < required_reflections; i++)
		{
			for (int j = 0; j < num_colour_tex; j++)
			{
				std::string uniform_name = "reflection_cubemaps[" + std::to_string((i * num_colour_tex) + j) + "]";
				this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), uniform_name, this->GetEngine()->GetTexture(GLTextureDataPreset::Black, TargetType::Texture_Cubemap));
			}
			this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), "reflection_depth_cubemaps[" + std::to_string(i) + "]", this->GetEngine()->GetTexture(GLTextureDataPreset::ZeroShadow, TargetType::Texture_Cubemap));
		}

		this->m_shader_program->SetUniform("reflection_count", static_cast<int>(reflections.size()));

		//skybox cubemap
		std::shared_ptr<Skybox> skybox = model->GetSkybox();
		std::shared_ptr<GLTexture> skybox_texture;
		if (skybox == nullptr)
		{
			skybox_texture = this->GetEngine()->GetTexture(GLTextureDataPreset::Black, TargetType::Texture_Cubemap);
		}
		else
		{
			std::shared_ptr<RenderTextureGroup> render_texture = this->GetEngine()->GetRenderTexture(skybox->GetReference());
			skybox_texture = render_texture->colour.at(0);
		}
		this->m_shader_program->SetTexture(static_cast<int>(model->GetReference()), "skyboxTexture", skybox_texture);
	}

	if (this->GetRenderMode() == RenderTargetMode::Wireframe)
	{
		this->m_shader_program->SetUniform("wireframe_colour", model->GetCurrentWireframeColour());
	}

	this->m_shader_program->Select(static_cast<int>(model->GetReference())); //select shader (and texture group)
}

void RenderTarget::Render_ForEachModel_FSQuad()
{
	this->m_shader_program->Select(-1); //select shader (and texture group)
}

void RenderTarget::DoClear() const
{
	glm::vec4 clear_colour = this->m_engine->GetScene()->GetClearColour();
	glClearColor(
		clear_colour.r,
		clear_colour.g,
		clear_colour.b,
		clear_colour.a
	);

	glClearDepth(1.0);

	if ((this->GetRenderMode() == RenderTargetMode::Normal_Draw)
		|| (this->RenderModeIsFSQuadRendering() && this->GetRenderMode() != RenderTargetMode::Normal_PostProcess))
	{
		glDepthMask(GL_FALSE);
	}
	else
	{
		glDepthMask(GL_TRUE);
	}

	if (this->GetRenderMode() == RenderTargetMode::Shadow)
	{
		glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
	}
	else
	{
		glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
	}

	if (this->m_config.clear_fbo && !this->RenderModeIsFSQuadRendering())
	{
		if ((this->GetRenderMode() == RenderTargetMode::Shadow) || (this->GetRenderMode() == RenderTargetMode::Normal_DepthOnly))
		{
			glClear(GL_DEPTH_BUFFER_BIT);
		}
		else if (this->GetRenderMode() == RenderTargetMode::Normal_Draw)
		{
			glClear(GL_COLOR_BUFFER_BIT);
		}
		else
		{
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		}
	}
}

void RenderTarget::CheckShaderValidity() const
{
	if (!this->m_shader_program->IsValid())
	{
		std::optional<std::string> validity_message = this->m_shader_program->GetProgramInfoLog();
		LogMessage("Shader not valid - info log: " + validity_message.value());
		throw std::runtime_error("Shader not valid - info log: " + validity_message.value());
	}
}

RenderTarget::RenderTarget(Engine* engine, RenderTargetConfig config) : m_engine(engine)
{
	this->m_cubemap_rotations.at(0) = glm::lookAt(glm::vec3(0.0f), glm::vec3(1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f));
	this->m_cubemap_rotations.at(1) = glm::lookAt(glm::vec3(0.0f), glm::vec3(-1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f));
	this->m_cubemap_rotations.at(2) = glm::lookAt(glm::vec3(0.0f), glm::vec3(0.0f, 1.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f));
	this->m_cubemap_rotations.at(3) = glm::lookAt(glm::vec3(0.0f), glm::vec3(0.0f, -1.0f, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f));
	this->m_cubemap_rotations.at(4) = glm::lookAt(glm::vec3(0.0f), glm::vec3(0.0f, 0.0f, 1.0f), glm::vec3(0.0f, -1.0f, 0.0f));
	this->m_cubemap_rotations.at(5) = glm::lookAt(glm::vec3(0.0f), glm::vec3(0.0f, 0.0f, -1.0f), glm::vec3(0.0f, -1.0f, 0.0f));

	this->m_engine->MakeContextCurrent(true); //this is necessary when constructing the first EngineCanvas - call it every time as construction is infrequent and already expensive

	this->m_render_function = [this](std::vector<Model*> model_pool)
	{
		return model_pool;
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

void RenderTarget::Render(std::vector<Model*> models, std::clock_t draw_time, bool continuous_draw)
{
	if (this->m_engine->GetScene() != nullptr)
	{
		if (this->m_fbo_contains_render && this->m_last_draw_was_continuous)
		{
			this->PostRenderEvent();
		}
		this->PreRenderEvent();

		std::vector<Model*> processed_models = this->m_render_function(models);
		this->RenderScene(processed_models, draw_time);

		if (!continuous_draw)
		{
			this->PostRenderEvent();
		}

		this->CheckParentContext();

		this->m_last_draw_was_continuous = continuous_draw;
	}
}

void RenderTarget::SetRenderFunction(ControllerFunction function)
{
	this->m_render_function = function;
}

std::unordered_set<RenderTextureReference> RenderTarget::GetRenderTextureDependencies() const
{
	std::unordered_set<RenderTextureReference> result;

	if (this->GetRenderMode() == RenderTargetMode::Normal_Draw)
	{
		for (const std::shared_ptr<PointLight>& pointlight : this->GetEngine()->GetScene()->GetPointLights())
		{
			if (this->m_config.Data<RenderTargetConfig::Normal_Draw>().draw_shadows)
			{
				result.insert(pointlight->GetReference());
			}
		}

		for (const std::shared_ptr<Reflection>& reflection : this->GetEngine()->GetScene()->GetReflections())
		{
			if (this->m_config.Data<RenderTargetConfig::Normal_Draw>().draw_reflections)
			{
				result.insert(reflection->GetReference());
			}
		}

		for (const std::shared_ptr<Skybox>& skybox : this->GetEngine()->GetScene()->GetSkyboxes())
		{
			result.insert(skybox->GetReference());
		}
	}

	return result;
}

void RenderTarget::CopyFrom(const RenderTarget* src)
{
	if ((this != src) && (this->GetFramebuffer() != src->GetFramebuffer()))
	{
		auto [src_x, src_y] = src->GetOutputSize();
		auto [dest_x, dest_y] = this->GetOutputSize();

		GLbitfield mask = GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT;

		glBindFramebuffer(GL_READ_FRAMEBUFFER, src->GetFramebuffer().GetFBO());
		glBindFramebuffer(GL_DRAW_FRAMEBUFFER, this->GetFramebuffer().GetFBO());

		glBlitFramebuffer(
			0, 0, static_cast<GLint>(src_x), static_cast<GLint>(src_y),
			0, 0, static_cast<GLint>(dest_x), static_cast<GLint>(dest_y),
			mask, GL_NEAREST
		);
	}
}

void RenderTarget::CopyTo(RenderTarget* dest) const
{
	dest->CopyFrom(this);
}

RenderTargetMode RenderTarget::GetRenderMode() const
{
	return this->m_config.GetMode();
}

void RenderTarget::SetConfig(RenderTargetConfig config)
{
	this->m_config = config;
	this->m_fbo_contains_render = false;

	if (this->GetRenderMode() == RenderTargetMode::Default)
	{
		std::vector<ShaderProgram::ShaderSource> shaders;
		shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_DEFAULT_FRAGSHADER), GL_FRAGMENT_SHADER));
		shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_DEFAULT_VERTSHADER), GL_VERTEX_SHADER));

		this->m_shader_program->SetShaderSources(shaders);
		this->m_shader_program->Recompile();
	}
	else
	{
		std::vector<ShaderProgram::ShaderSource> shaders;
		if (this->RenderModeIsModelRendering())
		{
			shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_VERTSHADER), GL_VERTEX_SHADER));
			shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_TESSCONTROLSHADER), GL_TESS_CONTROL_SHADER));
			shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_TESSEVALSHADER), GL_TESS_EVALUATION_SHADER));

			if (this->GetRenderMode() == RenderTargetMode::Normal_DepthOnly)
			{
				shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_GEOMSHADER), GL_GEOMETRY_SHADER));
				shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_NORMAL_DEPTHONLY_FRAGSHADER), GL_FRAGMENT_SHADER));
			}
			else if (this->GetRenderMode() == RenderTargetMode::Normal_Draw)
			{
				shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_GEOMSHADER), GL_GEOMETRY_SHADER));
				shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_MODEL_NORMAL_DRAW_FRAGSHADER), GL_FRAGMENT_SHADER));
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
		else if (this->RenderModeIsFSQuadRendering())
		{
			shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_POSTPROCESS_DEFAULT_VERTSHADER), GL_VERTEX_SHADER));
			shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_POSTPROCESS_DEFAULT_GEOMSHADER), GL_GEOMETRY_SHADER));

			if (this->GetRenderMode() == RenderTargetMode::PostProcess)
			{
				shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_POSTPROCESS_GENERIC_FRAGSHADER), GL_FRAGMENT_SHADER));
			}
			else if (this->GetRenderMode() == RenderTargetMode::Normal_PostProcess)
			{
				shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_POSTPROCESS_NORMAL_FRAGSHADER), GL_FRAGMENT_SHADER));
			}
			else if (this->GetRenderMode() == RenderTargetMode::Normal_SSRQuality)
			{
				shaders.push_back(ShaderProgram::ShaderSource(GetEmbeddedTextfile(RCID_TF_POSTPROCESS_NORMAL_SSRQUALITY_FRAGSHADER), GL_FRAGMENT_SHADER));
			}
		}

		this->m_shader_program->SetShaderSources(shaders);

		int num_colour_textures = 0;
		if (this->GetRenderMode() == RenderTargetMode::Normal_DepthOnly)
		{
			num_colour_textures = GetNumAttachedColourTextures(RenderTargetMode::Normal_Draw);
		}
		else
		{
			num_colour_textures = GetNumAttachedColourTextures(this->GetRenderMode());
		}
		this->m_shader_program->SetDefine("NUM_TEXTURES", num_colour_textures);

		this->m_shader_program->Recompile();

		this->m_shader_program->SetLabel("Shader program mode: " + GetRenderTargetModeName(this->GetRenderMode()));

		std::optional<std::string> program_info_log = this->m_shader_program->GetProgramInfoLog();
		if (program_info_log.has_value())
		{
			LogMessage("Shader program info log (" + GetRenderTargetModeName(this->GetRenderMode()) + "):\n" + program_info_log.value());
		}

		this->m_shader_program->AddUniformNames({
			//geometry
			"cubemap_transform[0]",
			"cubemap_transform[1]",
			"cubemap_transform[2]",
			"cubemap_transform[3]",
			"cubemap_transform[4]",
			"cubemap_transform[5]",
			"is_cubemap"
			});

		if (this->RenderModeIsModelRendering())
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
				"patch_size_v"
				});

			if (this->GetRenderMode() == RenderTargetMode::Normal_Draw)
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
					"render_output_depth",
					"render_output_dimensions"
					});

				for (int i = 0; i < GetNumColourTextures(RenderTargetMode::Normal_DepthOnly).value(); i++)
				{
					this->m_shader_program->AddUniformName("render_output_colour[" + std::to_string(i) + "]");
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
		else if (this->RenderModeIsFSQuadRendering())
		{
			if (this->GetRenderMode() == RenderTargetMode::Normal_PostProcess)
			{
				this->m_shader_program->AddUniformNames({
					//fragment
					"draw_frame_depth"
					});
			}
			else if (this->GetRenderMode() == RenderTargetMode::Normal_SSRQuality)
			{
				this->m_shader_program->AddUniformNames({
					//fragment
					"draw_frame_depth",
					"render_draw_output_dimensions"
					});
			}
			else if (this->GetRenderMode() == RenderTargetMode::PostProcess)
			{
				this->m_shader_program->AddUniformNames({
					//fragment
					"mode",
					// mode: box blur
					"modedata_BoxBlur.radius"
					});
			}
		}
	}
}

const RenderTargetConfig& RenderTarget::GetConfig() const
{
	return this->m_config;
}
