#include "Engine.h"

#include <algorithm>

#include <wx/image.h>

#include "LogMessage.h"
#include "PatchSize.h"

#include "scene/Scene.h"
#include "scene/Skybox.h"
#include "scene/model/Model.h"
#include "scene/model/Reflection.h"
#include "scene/light/PointLight.h"
#include "scene/texture/TextureFiltering.h"

#include "render/rendertarget/target/RenderTarget.h"
#include "render/rendertarget/canvas/EngineCanvas.h"

#include "render/controllers/RenderController.h"
#include "render/controllers/ShadowController.h"
#include "render/controllers/SkyboxController.h"
#include "render/controllers/ReflectionController.h"

#include "render/gltexture/GLTexture.h"
#include "render/gltexture/GLTextureFormat.h"
#include "render/gltexture/GLTextureType.h"

#include "render/glgeometry/GeometryVertexView.h"

#include "render/TargetType.h"

void GLAPIENTRY MessageCallback(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar* message, const void* userParam); //forward declaration to keep it out of the header

void Engine::LoadTexture(const Texture& texture)
{
	auto it = this->m_textures_static.find(texture.GetReference());
	bool texture_found = it != this->m_textures_static.end();
	
	bool reload_texture_data = false;
	if (texture_found)
	{
		if (std::get<1>(it->second) != texture)
		{
			reload_texture_data = true;
			std::get<1>(it->second) = texture;
		}
	}
	else
	{
		std::shared_ptr<GLTexture> loaded_texture = std::make_shared<GLTexture>(GLTexture::Preset::Colour, TargetType::Texture_2D, 3, texture.GetDimensions(), true);
		loaded_texture->SetMinFiltering(texture.GetMinFilter());
		loaded_texture->SetMagFiltering(texture.GetMagFilter());

		loaded_texture->SetLabel("Static colour texture (reference " + std::to_string(texture.GetReference()) + ")");

		this->m_textures_static.insert(std::pair(texture.GetReference(), std::tuple(loaded_texture, texture)));

		reload_texture_data = true;
	}

	if (reload_texture_data)
	{
		std::get<0>(this->m_textures_static.at(texture.GetReference()))->SetDimensions(texture.GetDimensions());
		std::get<0>(this->m_textures_static.at(texture.GetReference()))->SetPixels(GLTextureFormat_Colour(3, GLTextureType::UnsignedByte), std::vector<const void*>({ texture.GetData() }));
	}
}

void Engine::AddRenderController(RenderController* render_controller)
{
	this->m_render_controllers.push_back(std::unique_ptr<RenderController>(render_controller));
}

RenderController* Engine::GetRenderController(RenderTextureReference reference) const
{
	for (const std::unique_ptr<RenderController>& render_controller : this->m_render_controllers)
	{
		if (render_controller->GetReference() == reference)
		{
			return render_controller.get();
		}
	}
	return nullptr;
}

std::vector<RenderTextureReference> Engine::CollateRenderTextureDependencies(RenderTextureReference reference, const std::unordered_map<RenderTextureReference, std::unordered_set<RenderTextureReference>>& direct_dependencies, std::unordered_map<RenderTextureReference, bool>& is_drawn)
{
	std::vector<RenderTextureReference> result;

	for (RenderTextureReference inner_ref : direct_dependencies.at(reference))
	{
		if (!is_drawn.at(inner_ref))
		{
			is_drawn.at(inner_ref) = true;

			for (RenderTextureReference inner_inner_ref : this->CollateRenderTextureDependencies(inner_ref, direct_dependencies, is_drawn))
			{
				result.push_back(inner_inner_ref);
			}

			result.push_back(inner_ref);
		}
	}

	return result;
}

Engine::Engine(wxWindow* parent, Scene* scene, bool single_context_mode) : SceneChild(scene), m_parent(parent), m_single_context_mode(single_context_mode)
{
	{
		bool display_supported = false;
		int display_config_index = 0;

		while (!display_supported)
		{
			wxGLAttributes attributes;

			switch (display_config_index)
			{
			case 0: attributes.PlatformDefaults().Depth(32).Stencil(0).RGBA().DoubleBuffer().EndList(); break;
			case 1: attributes.PlatformDefaults().Depth(24).Stencil(8).RGBA().DoubleBuffer().EndList(); break;
			default: throw std::runtime_error("All display attributes have been tried and none of them are supported"); break;
			}

			if (wxGLCanvas::IsDisplaySupported(attributes))
			{
				this->m_canvas_args = attributes;
				display_supported = true;
			}
			else
			{
				display_config_index++;
			}
		}
	}

	this->m_glcontext_canvas = new wxGLCanvas(parent, this->m_canvas_args, wxID_ANY);

	{
		wxGLContextAttrs ctx_attrs;

		wxLogNull no_logging; //disable logging popups until this object is destroyed

#ifdef _DEBUG
		//test an impossible context to make sure wxGLContext::IsOK is working (this is done in the pyramid sample)
		ctx_attrs.PlatformDefaults().CoreProfile().MajorVersion(99).MinorVersion(2).EndList();
		this->m_glcontext = std::make_unique<wxGLContext>(this->m_glcontext_canvas, nullptr, &ctx_attrs);

		if (this->m_glcontext->IsOK())
		{
			throw std::runtime_error("Successfully created an impossible context - this should have failed");
		}

		ctx_attrs.Reset();
#endif
		//create the proper context
		ctx_attrs.PlatformDefaults().CoreProfile().MajorVersion(4).MinorVersion(3);

#ifdef _DEBUG
		ctx_attrs.DebugCtx();
#endif

		ctx_attrs.EndList();

		this->m_glcontext = std::make_unique<wxGLContext>(this->m_glcontext_canvas, nullptr, &ctx_attrs);

		if (!this->m_glcontext->IsOK())
		{
			throw std::runtime_error("OpenGL Context is not correct");
		}
	}

	this->m_glcontext_canvas->SetCurrent(*this->m_glcontext);

	std::remove(GAMEENGINE_LOG_PATH);
	LogMessage("New Engine created");

	glewExperimental = GL_TRUE;
	if (glewInit() != GLEW_OK)
	{
		throw std::runtime_error("Couldn't initialise glew");
	}

	glEnable(GL_DEBUG_OUTPUT);
	glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
	glDebugMessageCallback(MessageCallback, 0);

	LogMessage(std::string("Renderer: ") + reinterpret_cast<const char*>(glGetString(GL_RENDERER)) + " (" + reinterpret_cast<const char*>(glGetString(GL_VENDOR)) + ")" + '\n'
		+ std::string("Active OpenGL version: ") + reinterpret_cast<const char*>(glGetString(GL_VERSION)) + '\n'
		+ std::string("Active GLSL version: ") + reinterpret_cast<const char*>(glGetString(GL_SHADING_LANGUAGE_VERSION)));

	GLint max_patch_size = 32; //this is the minimum value required by the standard
	glGetIntegerv(GL_MAX_PATCH_VERTICES, &max_patch_size);

	//the range is open at this end, so the size of the patch must always be at least 1 less than the value returned
	//I just decrement the returned value so that it behaves "as it should" instead of dealing with this
	max_patch_size--;

	if (static_cast<std::size_t>(max_patch_size) < GAMEENGINE_PATCH_SIZE)
	{
		throw std::runtime_error("Patch size has been set to " + std::to_string(GAMEENGINE_PATCH_SIZE) + " vertices, but the implementation defined maximum is " + std::to_string(static_cast<int>(max_patch_size)));
	}
	else if (GAMEENGINE_PATCH_SIZE == 0)
	{
		throw std::runtime_error("Patch size must be greater than zero");
	}

	//patches - the standard doesn't let the same shader take different sized patches (even if NVIDIA does)
	glPatchParameteri(GL_PATCH_VERTICES, static_cast<GLint>(GAMEENGINE_PATCH_SIZE));

	this->MakeContextCurrent(true);

	glEnable(GL_TEXTURE_CUBE_MAP_SEAMLESS);

	glActiveTexture(GL_TEXTURE1);
}

EngineCanvasController* Engine::GenerateNewCanvas(std::vector<EngineCanvasController::CompositeLayer> composite_layers, wxWindowID id, wxWindow* parent)
{
	this->MakeContextCurrent();

	RenderTargetConfig empty_config; //configuration of the EngineCanvas is done by the EngineCanvasController
	EngineCanvas* canvas = new EngineCanvas(parent == nullptr ? this->m_parent : parent, id, this->m_canvas_args, this->m_glcontext.get(), this, empty_config);
	canvas->MakeOpenGLFocus();

	EngineCanvasController* controller = new EngineCanvasController(this, this->GetScene()->GetNewRenderTextureReference(), canvas, composite_layers);
	this->AddRenderController(controller);

	if (this->m_glcontext_canvas != nullptr)
	{
		this->m_glcontext_canvas->Destroy();
		this->m_glcontext_canvas = nullptr;
	}

	return controller;
}

EngineCanvasController* Engine::GenerateNewCanvas(std::vector<RenderMode> modes, wxWindowID id, wxWindow* parent)
{
	std::vector<EngineCanvasController::CompositeLayer> composite_layers;
	composite_layers.reserve(modes.size());
	for (RenderMode mode : modes)
	{
		EngineCanvasController::CompositeLayer layer;
		layer.mode = mode;
		composite_layers.push_back(layer);
	}

	return this->GenerateNewCanvas(composite_layers, id, parent);
}

EngineCanvasController* Engine::GenerateNewCanvas(RenderMode mode, wxWindowID id, wxWindow* parent)
{
	return this->GenerateNewCanvas(std::vector({ mode }), id, parent);
}

void Engine::Render(bool continuous_draw)
{
	if (this->GetScene() != nullptr)
	{
		this->MakeContextCurrent();

		//update static textures
		// load model textures
		for (const std::shared_ptr<Model>& model : this->GetScene()->GetModels())
		{
			this->LoadTexture(model->GetColourTexture());
			this->LoadTexture(model->GetNormalTexture());
			this->LoadTexture(model->GetSpecularTexture());
			this->LoadTexture(model->GetReflectionTexture());
			this->LoadTexture(model->GetSkyboxMaskTexture());
			this->LoadTexture(model->GetDisplacementTexture());
		}

		{
			enum class CubemapType
			{
				Reflection,
				PointLight,
				Skybox
			};

			//load required cubemaps and unload unused ones
			std::vector<std::tuple<RenderTextureReference, CubemapType>> cubemaps_to_add;
			std::vector<std::tuple<RenderTextureReference, CubemapType>> cubemaps_to_remove;
			{
				//perform diff for cubemap controllers
				std::vector<std::tuple<RenderTextureReference, CubemapType>> existing_cubemaps;
				std::vector<std::tuple<RenderTextureReference, CubemapType>> required_cubemaps;

				for (std::unique_ptr<RenderController>& render_controller : this->m_render_controllers)
				{
					if (render_controller->GetType() == RenderControllerType::Reflection)
					{
						existing_cubemaps.push_back(std::tuple(render_controller->GetReference(), CubemapType::Reflection));
					}
					else if (render_controller->GetType() == RenderControllerType::Shadow)
					{
						existing_cubemaps.push_back(std::tuple(render_controller->GetReference(), CubemapType::PointLight));
					}
					else if (render_controller->GetType() == RenderControllerType::Skybox)
					{
						existing_cubemaps.push_back(std::tuple(render_controller->GetReference(), CubemapType::Skybox));
					}
				}

				std::vector<std::tuple<std::shared_ptr<Cubemap>, CubemapType>> required_cubemaps_data;
				
				{
					for (const std::shared_ptr<Reflection>& reflection : this->GetScene()->GetReflections())
					{
						required_cubemaps_data.push_back(std::tuple(reflection, CubemapType::Reflection));
					}

					for (const std::shared_ptr<PointLight>& pointlight : this->GetScene()->GetPointLights())
					{
						required_cubemaps_data.push_back(std::tuple(pointlight, CubemapType::PointLight));
					}

					for (const std::shared_ptr<Skybox>& skybox : this->GetScene()->GetSkyboxes())
					{
						required_cubemaps_data.push_back(std::tuple(skybox, CubemapType::Skybox));
					}
				}

				for (const auto& [cubemap, cubemap_type] : required_cubemaps_data)
				{
					required_cubemaps.push_back(std::tuple(cubemap->GetReference(), cubemap_type));
				}

				std::sort(existing_cubemaps.begin(), existing_cubemaps.end());
				std::sort(required_cubemaps.begin(), required_cubemaps.end());

				int i = 0;
				int j = 0;
				while ((i < static_cast<int>(existing_cubemaps.size())) || (j < static_cast<int>(required_cubemaps.size())))
				{
					enum class State
					{
						Both,
						Added,
						Removed
					};

					State state;
					if (i == existing_cubemaps.size())
					{
						state = State::Added;
					}
					else if (j == required_cubemaps.size())
					{
						state = State::Removed;
					}
					else if (existing_cubemaps.at(i) == required_cubemaps.at(j))
					{
						state = State::Both;
					}
					else if (existing_cubemaps.at(i) < required_cubemaps.at(j))
					{
						state = State::Removed;
					}
					else
					{
						state = State::Added;
					}

					if (state == State::Added)
					{
						cubemaps_to_add.push_back(required_cubemaps.at(j));
						j++;
					}
					else if (state == State::Removed)
					{
						cubemaps_to_remove.push_back(existing_cubemaps.at(i));
						i++;
					}
					else if (state == State::Both)
					{
						i++;
						j++;
					}
				}
			}

			//remove old cubemap controllers
			{
				std::vector<int> cubemap_indices;
				for (const auto& [render_texture_reference, cubemap_type] : cubemaps_to_remove)
				{
					for (int i = 0; i < static_cast<int>(this->m_render_controllers.size()); i++)
					{
						if (this->m_render_controllers.at(i)->GetReference() == render_texture_reference)
						{
							cubemap_indices.push_back(i);
						}
					}
				}

				std::reverse(cubemap_indices.begin(), cubemap_indices.end()); //order high-low

				for (int i = 0; i < static_cast<int>(cubemap_indices.size()); i++)
				{
					this->m_render_controllers.erase(this->m_render_controllers.begin() + i);
				}
			}

			//create new cubemap controllers
			for (const auto& [reference, type] : cubemaps_to_add)
			{
				if (type == CubemapType::Reflection)
				{
					this->AddRenderController(new ReflectionController(this, reference));
				}
				else if (type == CubemapType::PointLight)
				{
					this->AddRenderController(new ShadowController(this, reference));
				}
				else if (type == CubemapType::Skybox)
				{
					this->AddRenderController(new SkyboxController(this, reference));
				}
				else
				{
					throw std::runtime_error("Invalid CubemapType enum: " + std::to_string(static_cast<int>(type)));
				}
			}
		}

		//perform any outstanding changes to loaded geometry
		{
			std::vector<ModelReference> to_remove;
			for (auto& [model_reference, loaded_geometries] : this->m_model_geometry)
			{
				std::optional<std::shared_ptr<Model>> model = this->GetScene()->GetModel(model_reference);

				if (model.has_value())
				{
					GeometryVertexView geometry_view = model.value()->GetGeometry();
					//work out what geometry should be loaded
					std::set<Geometry::RenderInfo> target_geometry_infos = geometry_view.GetRenderInfos();

					//work out what geometry actually is loaded
					std::set<Geometry::RenderInfo> current_geometry_infos;
					for (const auto& [render_info, glgeometry] : loaded_geometries)
					{
						current_geometry_infos.insert(render_info);
					}

					//check if keys are in both maps
					std::set<Geometry::RenderInfo> geometry_to_add;
					std::set<Geometry::RenderInfo> geometry_to_remove;

					//work out the render info for the geometry that needs to be added
					std::set_difference(target_geometry_infos.begin(), target_geometry_infos.end(), current_geometry_infos.begin(), current_geometry_infos.end(), std::inserter(geometry_to_add, geometry_to_add.begin()));

					//work out the render info for the geometry that needs to be removed
					std::set_difference(current_geometry_infos.begin(), current_geometry_infos.end(), target_geometry_infos.begin(), target_geometry_infos.end(), std::inserter(geometry_to_remove, geometry_to_remove.begin()));

					//remove excess geometry
					for (const Geometry::RenderInfo& render_info : geometry_to_remove)
					{
						loaded_geometries.erase(render_info);
					}

					//update existing geometry if required
					for (auto& [render_info, loaded_geometry] : loaded_geometries)
					{
						if (!geometry_view.IsEqual(loaded_geometry->GetValues(), render_info))
						{
							loaded_geometry->SetData(geometry_view.GetVerticesFromRenderInfo(render_info), render_info.primitive_size, render_info.primitive_type);
						}
					}

					//add new geometry
					for (const Geometry::RenderInfo& render_info : geometry_to_add)
					{
						std::shared_ptr<GLGeometry> geometry = std::make_shared<GLGeometry>(geometry_view.GetVerticesFromRenderInfo(render_info), render_info.primitive_size, render_info.primitive_type);
						geometry->SetLabel(model.value()->GetIdentifier() + ": " + GetPrimitiveTypeName(render_info.primitive_type));

						loaded_geometries.insert(std::pair(render_info, std::move(geometry)));
					}
				}
				else
				{
					to_remove.push_back(model_reference);
				}
			}

			//remove old models
			for (ModelReference model_reference : to_remove)
			{
				this->m_model_geometry.erase(model_reference);
			}

			//add new models
			for (const std::shared_ptr<Model>& model : this->GetScene()->GetModels())
			{
				if (this->m_model_geometry.count(model->GetReference()) == 0)
				{
					GeometryVertexView cpu_geometry_view = model->GetGeometry();
					std::unordered_map<Geometry::RenderInfo, std::shared_ptr<GLGeometry>, Geometry::RenderInfo::Hash> gpu_geometry;
					for (const Geometry::RenderInfo& render_info : cpu_geometry_view.GetRenderInfos())
					{
						std::shared_ptr<GLGeometry> geometry = std::make_shared<GLGeometry>(cpu_geometry_view.GetVerticesFromRenderInfo(render_info), render_info.primitive_size, render_info.primitive_type);
						geometry->SetLabel(model->GetIdentifier() + ": " + GetPrimitiveTypeName(render_info.primitive_type));

						gpu_geometry.insert(std::pair(render_info, std::move(geometry)));
					}

					this->m_model_geometry.insert(std::pair(model->GetReference(), gpu_geometry));
				}
			}
		}

		//draw required render controllers
		{
			std::unordered_map<RenderTextureReference, bool> draw_required;
			std::unordered_map<RenderTextureReference, std::unordered_set<RenderTextureReference>> reference_direct_dependencies;
			std::vector<RenderController*> essential_draws;

			for (std::unique_ptr<RenderController>& render_controller : this->m_render_controllers)
			{
				bool is_essential_draw = render_controller->IsEssentialDraw();
				draw_required.insert(std::pair(render_controller->GetReference(), is_essential_draw));
				reference_direct_dependencies.insert(std::pair(render_controller->GetReference(), render_controller->GetRenderTextureDependencies()));

				if (is_essential_draw)
				{
					essential_draws.push_back(render_controller.get());
				}
			}
			
			std::vector<RenderController*> to_draw;
			for (RenderController* render_controller : essential_draws)
			{
				for (RenderTextureReference ref : this->CollateRenderTextureDependencies(render_controller->GetReference(), reference_direct_dependencies, draw_required))
				{
					to_draw.push_back(this->GetRenderController(ref));
				}
				to_draw.push_back(render_controller);
			}

			//tell controllers to redraw themselves (if required)
			for (RenderController* render_controller : to_draw)
			{
				render_controller->Render(continuous_draw);
			}

			for (RenderController* render_controller : to_draw)
			{
				render_controller->PostRender();
			}
		}
	}
}

std::shared_ptr<GLTexture> Engine::GetTexture(TextureReference reference) const
{
	return std::get<0>(this->m_textures_static.at(reference));
}

std::shared_ptr<GLTexture> Engine::GetTexture(const Texture& texture) const
{
	return std::get<0>(this->m_textures_static.at(texture.GetReference()));
}

std::shared_ptr<GLTexture> Engine::GetTexture(GLTextureDataPreset preset, TargetType target)
{
	return this->GetTexture(GLTexturePreset(target, preset));
}

std::shared_ptr<GLTexture> Engine::GetTexture(GLTexturePreset preset)
{
	if (this->m_textures_static_presets.count(preset) == 0)
	{
		std::shared_ptr<GLTexture> texture = std::make_shared<GLTexture>(preset);
		texture->SetPixels(preset.preset);
		texture->SetLabel("Texture preset (" + preset.ToString() + ")");
		this->m_textures_static_presets.insert(std::pair(preset, texture));
	}

	return this->m_textures_static_presets.at(preset);
}

std::shared_ptr<RenderTextureGroup> Engine::GetRenderTexture(RenderTextureReference reference) const
{
	for (const std::unique_ptr<RenderController>& controller : this->m_render_controllers)
	{
		if (controller->GetReference() == reference)
		{
			return controller->GetRenderTexture();
		}
	}

	throw std::invalid_argument("Couldn't resolve render texture reference " + std::to_string(reference));
}

void Engine::DrawModel(Model* model, std::function<GLenum(Geometry::RenderInfo info, const GLGeometry& loaded_geometry)> predraw)
{
	this->MakeContextCurrent();

	std::vector<std::tuple<Geometry::RenderInfo, std::shared_ptr<GLGeometry>>> geometry;

	if (this->IsChildOfSameScene(model))
	{
		for (auto& [render_info, loaded_geometry] : this->m_model_geometry.at(model->GetReference()))
		{
			geometry.push_back(std::tuple(render_info, loaded_geometry));
		}
	}

	for (std::shared_ptr<Geometry>& geom : model->GetGeometry())
	{
		std::shared_ptr<PresetGeometry> preset_geom = std::dynamic_pointer_cast<PresetGeometry>(geom);

		if (preset_geom.get() != nullptr)
		{
			auto it = this->m_geometry_presets.find(preset_geom->GetGeometryType());
			if (it == this->m_geometry_presets.end())
			{
				std::tuple data = std::tuple(preset_geom->GetRenderInfo(), std::make_shared<GLGeometry>(preset_geom->GetPrimitives(), preset_geom->GetPrimitiveSize(), preset_geom->GetPrimitiveType()));
				std::get<1>(data)->SetLabel("Preset geometry: " + GetPresetGeometryType(preset_geom->GetGeometryType()));

				this->m_geometry_presets.insert(std::pair(preset_geom->GetGeometryType(), data));
				geometry.push_back(data);
			}
			else
			{
				auto& [render_info, glgeometry] = it->second;
				geometry.push_back(std::tuple(render_info, glgeometry));
			}
		}
	}

	for (auto& [render_info, loaded_geometry] : geometry)
	{
		GLenum render_mode = predraw(render_info, *loaded_geometry);

		loaded_geometry->Draw(render_mode);
	}

	glBindVertexArray(NULL);
	glBindBuffer(GL_ARRAY_BUFFER, NULL);
}

void Engine::PrunePresetGeometry(PresetGeometry::GeometryType type)
{
	auto it = this->m_geometry_presets.find(type);
	if (it != this->m_geometry_presets.end())
	{
		this->m_geometry_presets.erase(it);
	}
}

void Engine::MakeContextCurrent(bool force) const
{
	if (force || !this->m_single_context_mode)
	{
		if (this->m_glcontext_canvas == nullptr)
		{
			bool context_set = false;
			for (const std::unique_ptr<RenderController>& render_controller : this->m_render_controllers)
			{
				if (!context_set)
				{
					if (render_controller->GetType() == RenderControllerType::EngineCanvas)
					{
						EngineCanvasController* engine_canvas_controller = dynamic_cast<EngineCanvasController*>(render_controller.get());
						engine_canvas_controller->GetEngineCanvas()->MakeOpenGLFocus();
						context_set = true;
					}
				}
			}

			if (!context_set)
			{
				throw std::runtime_error("Unable to set context as current");
			}
		}
		else
		{
			this->m_glcontext_canvas->SetCurrent(*this->m_glcontext);
		}
	}
}

bool Engine::ContextIsValid() const
{
	return this->m_glcontext->IsOK();
}

void Engine::SetDebugMessageLevel(Engine::DebugMessageConfig config) const
{
	this->MakeContextCurrent();
	glDebugMessageControl(config.source, config.type, config.severity, 0, nullptr, config.enabled ? GL_TRUE : GL_FALSE);
}

void Engine::SetDebugMessageLevel(std::vector<Engine::DebugMessageConfig> config) const
{
	this->MakeContextCurrent();
	for (Engine::DebugMessageConfig& config_detail : config)
	{
		glDebugMessageControl(config_detail.source, config_detail.type, config_detail.severity, 0, nullptr, config_detail.enabled ? GL_TRUE : GL_FALSE);
	}
}

void GLAPIENTRY MessageCallback(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar* message, const void* userParam)
{
	std::string source_str;
	switch (source)
	{
		//extracted from glew.h
	case GL_DEBUG_SOURCE_API: source_str = "API"; break;
	case GL_DEBUG_SOURCE_WINDOW_SYSTEM: source_str = "window system"; break;
	case GL_DEBUG_SOURCE_SHADER_COMPILER: source_str = "shader compiler"; break;
	case GL_DEBUG_SOURCE_THIRD_PARTY: source_str = "third party"; break;
	case GL_DEBUG_SOURCE_APPLICATION: source_str = "application"; break;
	case GL_DEBUG_SOURCE_OTHER: source_str = "other"; break;
	default: source_str = "unknown"; break;
	}

	std::string err_type;
	switch (type) //https://www.khronos.org/opengl/wiki/OpenGL_Error#Meaning_of_errors
	{
		//listed errors
	case GL_INVALID_ENUM: err_type = "invalid enum"; break;
	case GL_INVALID_VALUE: err_type = "invalid value"; break;
	case GL_INVALID_OPERATION: err_type = "invalid operation"; break;
	case GL_STACK_OVERFLOW: err_type = "stack overflow"; break;
	case GL_STACK_UNDERFLOW: err_type = "stack underflow"; break;
	case GL_OUT_OF_MEMORY: err_type = "out of memory"; break;
	case GL_INVALID_FRAMEBUFFER_OPERATION: err_type = "invalid framebuffer operation"; break;
	case GL_CONTEXT_LOST: err_type = "context lost"; break;
	//case GL_TABLE_TOO_LARGE: err_type = "table too large"; break; //deprecated in 3.0 core, removed in 3.1 core and above

		//errors start with 0x05 - these names were extracted from glew.h
#if GL_INVALID_FRAMEBUFFER_OPERATION_EXT != GL_INVALID_FRAMEBUFFER_OPERATION
	case GL_INVALID_FRAMEBUFFER_OPERATION_EXT: err_type = "invalid framebuffer operation EXT"; break;
#endif

		//debug messages - extracted from glew.h
	case GL_DEBUG_TYPE_ERROR: err_type = "debug error"; break;
	case GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR: err_type = "deprecated behaviour"; break;
	case GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR: err_type = "undefined behaviour"; break;
	case GL_DEBUG_TYPE_PORTABILITY: err_type = "portability"; break;
	case GL_DEBUG_TYPE_PERFORMANCE: err_type = "performance"; break;
	case GL_DEBUG_TYPE_OTHER: err_type = "other"; break;
	case GL_DEBUG_TYPE_MARKER: err_type = "marker"; break;

		//default
	default: err_type = std::to_string(type) + " - unknown"; break;
	}

	std::string log_message = "[source: " + source_str + ", type: " + err_type + ", severity: ";

	switch (severity)
	{
	case GL_DEBUG_SEVERITY_HIGH: log_message += "high"; break;
	case GL_DEBUG_SEVERITY_MEDIUM: log_message += "medium"; break;
	case GL_DEBUG_SEVERITY_LOW: log_message += "low"; break;
	case GL_DEBUG_SEVERITY_NOTIFICATION: log_message += "notification"; break;
	default: log_message += severity; break;
	}

	log_message += "] " + std::string(message);

	LogMessage(log_message, false);

	if (severity == GL_DEBUG_SEVERITY_HIGH
		|| type == GL_INVALID_ENUM
		|| type == GL_INVALID_VALUE
		|| type == GL_INVALID_OPERATION
		|| type == GL_STACK_OVERFLOW
		|| type == GL_STACK_UNDERFLOW
		|| type == GL_OUT_OF_MEMORY
		|| type == GL_INVALID_FRAMEBUFFER_OPERATION
		|| type == GL_CONTEXT_LOST
		|| type == GL_DEBUG_TYPE_ERROR
		|| type == GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR)
	{
		throw std::runtime_error(message);
	}
}