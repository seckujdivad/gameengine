#include "Engine.h"

#include <tuple>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <chrono>

#include <wx/image.h>

#include "scene/Scene.h"

#include "render/Renderable.h"
#include "render/EngineCanvas.h"

#include "render/controllers/RenderController.h"
#include "render/controllers/ShadowController.h"
#include "render/controllers/SkyboxController.h"
#include "render/controllers/ReflectionController.h"

const char GAMEENGINE_LOG_PATH[] = "gameengine_GL.log";

const std::size_t GAMEENGINE_PATCH_SIZE = 16;

void GLAPIENTRY MessageCallback(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar* message, const void* userParam); //forward declaration to keep it out of the header

void Engine::LoadTexture(LocalTexture texture, std::string uniform_name)
{
	auto it = this->m_textures_static.find(texture.GetReference());
	GLuint texture_id;
	if (it == this->m_textures_static.end())
	{
		glGenTextures(1, &texture_id);
		glBindTexture(GL_TEXTURE_2D, texture_id);

		//wrapping
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

		//filter
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, texture.GetMinFilter() == LocalTexture::Filter::Nearest ? GL_NEAREST : GL_LINEAR); //shrinking filter
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, texture.GetMagFilter() == LocalTexture::Filter::Nearest ? GL_NEAREST : GL_LINEAR); //enlarging filter

		LoadedTexture texture_data;
		texture_data.id = texture_id;
		texture_data.type = GL_TEXTURE_2D;
		texture_data.uniform_name = uniform_name;

		this->m_textures_static.insert(std::pair(texture.GetReference(), std::tuple(texture_data, texture)));
	}
	else
	{
		glBindTexture(GL_TEXTURE_2D, std::get<0>(it->second).id);
	}

	if ((it == this->m_textures_static.end()) || (std::get<1>(it->second) != texture))
	{
		std::tuple dimensions = texture.GetDimensions();
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, std::get<0>(dimensions), std::get<1>(dimensions), 0, GL_RGB, GL_UNSIGNED_BYTE, texture.GetData());
		glGenerateMipmap(GL_TEXTURE_2D);
	}
}

std::unordered_map<Geometry::RenderInfo, std::vector<GLfloat>, Geometry::RenderInfo::Hash> Engine::GenerateGeometryGroups(std::vector<std::shared_ptr<Geometry>> geometry)
{
	std::unordered_map<Geometry::RenderInfo, std::vector<GLfloat>, Geometry::RenderInfo::Hash> result;

	for (const std::shared_ptr<Geometry>& inner_geometry : geometry)
	{
		if (std::dynamic_pointer_cast<PresetGeometry>(inner_geometry).get() == nullptr)
		{
			Geometry::RenderInfo render_info = inner_geometry->GetRenderInfo();
			std::vector<double> data_highp = inner_geometry->GetPrimitives();

			std::vector<GLfloat> vertices;
			vertices.reserve(data_highp.size());
			for (double value : data_highp)
			{
				vertices.push_back(static_cast<GLfloat>(value));
			}

			auto it = result.find(render_info);
			if (it == result.end())
			{
				result.insert(std::pair(render_info, vertices));
			}
			else
			{
				for (GLfloat value : vertices)
				{
					it->second.push_back(value);
				}
			}
		}
	}

	return result;
}

std::unordered_map<Geometry::RenderInfo, Engine::LoadedGeometry, Geometry::RenderInfo::Hash> Engine::LoadGeometry(std::vector<std::shared_ptr<Geometry>> geometry)
{
	std::unordered_map<Geometry::RenderInfo, Engine::LoadedGeometry, Geometry::RenderInfo::Hash> result;

	for (auto& [render_info, vertices] : this->GenerateGeometryGroups(geometry))
	{
		result.insert(std::pair(render_info, this->CreateLoadedGeometry(vertices, render_info.primitive_size, render_info.primitive_type)));
	}

	return result;
}

Engine::LoadedGeometry Engine::CreateLoadedGeometry(std::vector<GLfloat> vertices, std::size_t primitive_size, Geometry::PrimitiveType primitive_type)
{
	LoadedGeometry loaded_geometry;
	loaded_geometry.data = vertices;

	std::vector<GLfloat> padded_vertices;

	if (primitive_size > GAMEENGINE_PATCH_SIZE)
	{
		throw std::invalid_argument("Primitives must have less than " + std::to_string(GAMEENGINE_PATCH_SIZE) + " vertices");
	}

#ifdef _DEBUG
	if (loaded_geometry.data.size() % static_cast<std::size_t>(GAMEENGINE_VALUES_PER_VERTEX) != 0)
	{
		throw std::runtime_error("Incomplete vertices provided");
	}
#endif

	if ((primitive_type == Geometry::PrimitiveType::Patches || primitive_type == Geometry::PrimitiveType::Quads)
		&& primitive_size != GAMEENGINE_PATCH_SIZE)
	{
		std::size_t num_primitives = vertices.size() / (primitive_size * GAMEENGINE_VALUES_PER_VERTEX);
		padded_vertices.reserve(num_primitives * GAMEENGINE_PATCH_SIZE * GAMEENGINE_VALUES_PER_VERTEX);
		for (std::size_t i = 0; i < num_primitives; i++)
		{
			for (std::size_t j = 0; j < primitive_size * GAMEENGINE_VALUES_PER_VERTEX; j++)
			{
				padded_vertices.push_back(vertices.at((i * primitive_size * GAMEENGINE_VALUES_PER_VERTEX) + j));
			}

			for (std::size_t j = 0; j < GAMEENGINE_PATCH_SIZE - primitive_size; j++)
			{
				for (std::size_t k = 0; k < std::size_t(GAMEENGINE_VALUES_PER_VERTEX); k++)
				{
					padded_vertices.push_back(0.0f);
				}
			}
		}
	}
	else
	{
		padded_vertices = vertices;
	}

	loaded_geometry.buffer_len = static_cast<GLsizei>(padded_vertices.size());

#ifdef _DEBUG
	if (loaded_geometry.buffer_len % static_cast<std::size_t>(GAMEENGINE_VALUES_PER_VERTEX) != 0)
	{
		throw std::runtime_error("Incomplete vertices generated");
	}
#endif

	//create vao and vbo
	glGenVertexArrays(1, &loaded_geometry.vao);
	glBindVertexArray(loaded_geometry.vao);

	glGenBuffers(1, &loaded_geometry.vbo);
	glBindBuffer(GL_ARRAY_BUFFER, loaded_geometry.vbo);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * padded_vertices.size(), padded_vertices.data(), GL_DYNAMIC_DRAW);

	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, GAMEENGINE_VALUES_PER_VERTEX * sizeof(GLfloat), 0);
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, GAMEENGINE_VALUES_PER_VERTEX * sizeof(GLfloat), (void*)(3 * sizeof(GLfloat)));
	glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, GAMEENGINE_VALUES_PER_VERTEX * sizeof(GLfloat), (void*)(6 * sizeof(GLfloat)));

	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glEnableVertexAttribArray(2);

	glBindVertexArray(NULL);
	glBindBuffer(GL_ARRAY_BUFFER, NULL);

	return loaded_geometry;
}

void Engine::AddRenderController(RenderController* render_controller)
{
	this->m_render_controllers.push_back(render_controller);
}

RenderController* Engine::GetRenderController(RenderTextureReference reference)
{
	for (RenderController* render_controller : this->m_render_controllers)
	{
		if (render_controller->GetReference() == reference)
		{
			return render_controller;
		}
	}
	return nullptr;
}

std::vector<RenderTextureReference> Engine::CollateRenderTextureDependencies(RenderTextureReference reference, std::unordered_map<RenderTextureReference, std::unordered_set<RenderTextureReference>>& direct_dependencies, std::unordered_map<RenderTextureReference, bool>& is_drawn)
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

Engine::Engine(wxWindow* parent, Scene* scene) : SceneChild(scene), m_parent(parent)
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
		this->m_glcontext = new wxGLContext(this->m_glcontext_canvas, NULL, &ctx_attrs);

		if (this->m_glcontext->IsOK())
		{
			throw std::runtime_error("Successfully created an impossible context - this should have failed");
		}

		delete this->m_glcontext;
		ctx_attrs.Reset();
#endif
		//create the proper context
		ctx_attrs.PlatformDefaults().CoreProfile().MajorVersion(4).MinorVersion(3);

#ifdef _DEBUG
		ctx_attrs.DebugCtx();
#endif

		ctx_attrs.EndList();

		this->m_glcontext = new wxGLContext(this->m_glcontext_canvas, NULL, &ctx_attrs);

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

	glLoadIdentity();

	glEnable(GL_DEBUG_OUTPUT);
	glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
	glDebugMessageCallback(MessageCallback, 0);

	LogMessage(std::string("Renderer: ") + reinterpret_cast<const char*>(glGetString(GL_RENDERER)) + " (" + reinterpret_cast<const char*>(glGetString(GL_VENDOR)) + ")" + '\n'
		+ std::string("Active OpenGL version: ") + reinterpret_cast<const char*>(glGetString(GL_VERSION)) + '\n'
		+ std::string("Active GLSL version: ") + reinterpret_cast<const char*>(glGetString(GL_SHADING_LANGUAGE_VERSION)));


	//patches - the standard doesn't let the same shader take different sized patches (even if NVIDIA does)
	glPatchParameteri(GL_PATCH_VERTICES, static_cast<GLint>(GAMEENGINE_PATCH_SIZE));

#ifdef _DEBUG
	{
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
	}
#endif
}

Engine::~Engine()
{
	for (RenderController* render_controller : this->m_render_controllers)
	{
		delete render_controller;
	}

	for (const auto& [reference, loaded_texture] : this->m_textures_static)
	{
		glDeleteTextures(1, &std::get<0>(loaded_texture).id);
	}

	for (const auto& [reference, loaded_geometries] : this->m_model_geometry)
	{
		for (auto& [render_info, loaded_geometry] : loaded_geometries)
		{
			loaded_geometry.FreeGL();
		}
	}

	for (const auto& [geometry_type, render_data] : this->m_geometry_presets)
	{
		std::get<1>(render_data).FreeGL();
	}

	delete this->m_glcontext;
}

EngineCanvasController* Engine::GenerateNewCanvas(std::vector<EngineCanvasController::CompositeLayer> composite_layers, wxWindowID id, wxWindow* parent)
{
	RenderableConfig empty_config; //configuration of the EngineCanvas is done by the EngineCanvasController
	EngineCanvas* canvas = new EngineCanvas(parent == nullptr ? this->m_parent : parent, id, this->m_canvas_args, this->m_glcontext, this, empty_config);
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

EngineCanvasController* Engine::GenerateNewCanvas(std::vector<RenderableConfig> configs, wxWindowID id, wxWindow* parent)
{
	std::vector<EngineCanvasController::CompositeLayer> composite_layers;
	composite_layers.reserve(configs.size());
	for (const RenderableConfig& config : configs)
	{
		EngineCanvasController::CompositeLayer layer;
		layer.config = config;
		composite_layers.push_back(layer);
	}

	return this->GenerateNewCanvas(composite_layers, id, parent);
}

EngineCanvasController* Engine::GenerateNewCanvas(RenderableConfig config, wxWindowID id, wxWindow* parent)
{
	return this->GenerateNewCanvas(std::vector({ config }), id, parent);
}

void Engine::Render()
{
	if (this->GetScene() != nullptr)
	{
		this->MakeContextCurrent();

		//update static textures
		// load model textures
		for (Model* model : this->GetScene()->GetModels())
		{
			this->LoadTexture(model->GetColourTexture(), "colourTexture");
			this->LoadTexture(model->GetNormalTexture(), "normalTexture");
			this->LoadTexture(model->GetSpecularTexture(), "specularTexture");
			this->LoadTexture(model->GetReflectionTexture(), "reflectionIntensityTexture");
			this->LoadTexture(model->GetSkyboxMaskTexture(), "skyboxMaskTexture");
			this->LoadTexture(model->GetDisplacementTexture(), "displacementTexture");
		}

		//load required cubemaps and unload unused ones
		std::vector<std::tuple<RenderTextureReference, CubemapType>> cubemaps_to_add;
		std::vector<std::tuple<RenderTextureReference, CubemapType>> cubemaps_to_remove;
		{
			//perform diff for cubemap controllers
			std::vector<std::tuple<RenderTextureReference, CubemapType>> existing_cubemaps;
			std::vector<std::tuple<RenderTextureReference, CubemapType>> required_cubemaps;

			for (RenderController* render_controller : this->m_render_controllers)
			{
				if (render_controller->GetType() == RenderControllerType::Reflection)
				{
					existing_cubemaps.push_back(std::tuple(render_controller->GetReference(), CubemapType::Reflection));
				}
				else if (render_controller->GetType() == RenderControllerType::Shadow)
				{
					existing_cubemaps.push_back(std::tuple(render_controller->GetReference(), CubemapType::Pointlight));
				}
				else if (render_controller->GetType() == RenderControllerType::Skybox)
				{
					existing_cubemaps.push_back(std::tuple(render_controller->GetReference(), CubemapType::Skybox));
				}
			}

			std::vector<std::tuple<Cubemap*, CubemapType>> required_cubemap_ptrs = this->GetScene()->GetCubemaps();
			for (const auto& [cubemap, cubemap_type] : required_cubemap_ptrs)
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
				delete this->m_render_controllers.at(i);
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
			else if (type == CubemapType::Pointlight)
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

		//remove non-existent geometry and update existing (if required)
		{
			std::vector<ModelReference> to_remove;
			for (auto& [model_reference, loaded_geometries] : this->m_model_geometry)
			{
				Model* model = this->GetScene()->GetModel(model_reference);

				if (model == nullptr)
				{
					to_remove.push_back(model_reference);
				}
				else
				{
					std::unordered_map<Geometry::RenderInfo, std::vector<GLfloat>, Geometry::RenderInfo::Hash> geometry_groups = this->GenerateGeometryGroups(model->GetGeometry());

					//check if keys are in both maps
					std::vector<Geometry::RenderInfo> geometry_to_add;
					std::vector<Geometry::RenderInfo> geometry_to_remove;

					//work out the render info for the geometry that needs to be added
					for (auto& [render_info, vertices] : geometry_groups)
					{
						if (loaded_geometries.count(render_info) == 0)
						{
							geometry_to_add.push_back(render_info);
						}
					}

					//work out the render info for the geometry that needs to be removed
					for (auto& [render_info, loaded_geometry] : loaded_geometries)
					{
						if (geometry_groups.count(render_info) == 0)
						{
							geometry_to_remove.push_back(render_info);
						}
					}

					//remove excess geometry
					for (const Geometry::RenderInfo& render_info : geometry_to_remove)
					{
						loaded_geometries.at(render_info).FreeGL();
						loaded_geometries.erase(render_info);
					}

					//add new geometry
					for (const Geometry::RenderInfo& render_info : geometry_to_add)
					{
						loaded_geometries.insert(std::pair(render_info, this->CreateLoadedGeometry(geometry_groups.at(render_info), render_info.primitive_size, render_info.primitive_type)));
					}

					//update existing geometry if required
					for (auto& [render_info, loaded_geometry] : loaded_geometries)
					{
						if (loaded_geometry.data != geometry_groups.at(render_info))
						{
							loaded_geometry.data = geometry_groups.at(render_info);

							glBindVertexArray(loaded_geometry.vao);
							glBindBuffer(GL_ARRAY_BUFFER, loaded_geometry.vbo);

							glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat)* loaded_geometry.data.size(), loaded_geometry.data.data(), GL_DYNAMIC_DRAW);

							glBindVertexArray(NULL);
							glBindBuffer(GL_ARRAY_BUFFER, NULL);
						}
					}
				}
			}

			//remove old geometry
			for (ModelReference model_reference : to_remove)
			{
				for (auto& [render_info, loaded_geometry] : this->m_model_geometry.at(model_reference))
				{
					loaded_geometry.FreeGL();
				}
				
				this->m_model_geometry.erase(model_reference);
			}

			//add new geometry
			for (Model* model : this->GetScene()->GetModels())
			{
				if (this->m_model_geometry.count(model->GetReference()) == 0)
				{
					this->m_model_geometry.insert(std::pair(model->GetReference(), this->LoadGeometry(model->GetGeometry())));
				}
			}
		}

		//draw required render controllers
		{
			std::unordered_map<RenderTextureReference, bool> draw_required;
			std::unordered_map<RenderTextureReference, std::unordered_set<RenderTextureReference>> reference_direct_dependencies;
			std::vector<RenderController*> essential_draws;

			for (RenderController* render_controller : this->m_render_controllers)
			{
				bool is_essential_draw = render_controller->IsEssentialDraw();
				draw_required.insert(std::pair(render_controller->GetReference(), is_essential_draw));
				reference_direct_dependencies.insert(std::pair(render_controller->GetReference(), render_controller->GetRenderTextureDependencies()));

				if (is_essential_draw)
				{
					essential_draws.push_back(render_controller);
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
				render_controller->Render();
			}

			for (RenderController* render_controller : to_draw)
			{
				render_controller->PostRender();
			}
		}
	}

	glFlush();
}

LoadedTexture Engine::GetTexture(TextureReference reference) const
{
	return std::get<0>(this->m_textures_static.at(reference));
}

RenderTextureGroup Engine::GetRenderTexture(RenderTextureReference reference) const
{
	for (RenderController* controller : this->m_render_controllers)
	{
		if (controller->GetReference() == reference)
		{
			return controller->GetRenderTexture();
		}
	}

	throw std::invalid_argument("Couldn't resolve render texture reference " + std::to_string(reference));
}

void Engine::DrawModel(Model* model, std::function<GLenum(Geometry::RenderInfo info, const LoadedGeometry& loaded_geometry)> predraw)
{
	std::vector<std::tuple<Geometry::RenderInfo, Engine::LoadedGeometry>> geometry;

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
				std::vector<GLfloat> vertices;
				std::vector<double> vertices_highp = preset_geom->GetPrimitives();
				vertices.reserve(vertices_highp.size());
				for (double value : vertices_highp)
				{
					vertices.push_back(static_cast<GLfloat>(value));
				}

				std::tuple data = std::tuple(preset_geom->GetRenderInfo(), this->CreateLoadedGeometry(vertices, preset_geom->GetPrimitiveSize(), preset_geom->GetPrimitiveType()));

				this->m_geometry_presets.insert(std::pair(preset_geom->GetGeometryType(), data));
				geometry.push_back(data);
			}
			else
			{
				geometry.push_back(it->second);
			}
		}
	}

	for (auto& [render_info, loaded_geometry] : geometry)
	{
		GLenum render_mode = predraw(render_info, loaded_geometry);

		this->BindVAO(loaded_geometry);

		GLsizei num_elements = static_cast<GLsizei>(loaded_geometry.buffer_len / static_cast<std::size_t>(GAMEENGINE_VALUES_PER_VERTEX));
		glDrawArrays(render_mode, 0, num_elements);
	}

	glBindVertexArray(NULL);
	glBindBuffer(GL_ARRAY_BUFFER, NULL);
}

void Engine::BindVAO(LoadedGeometry loaded_geometry)
{
	glBindVertexArray(loaded_geometry.vao);
	glBindBuffer(GL_ARRAY_BUFFER, loaded_geometry.vbo);
}

void Engine::PrunePresetGeometry(PresetGeometry::GeometryType type)
{
	auto it = this->m_geometry_presets.find(type);
	if (it != this->m_geometry_presets.end())
	{
		std::get<1>(it->second).FreeGL();
		this->m_geometry_presets.erase(it);
	}
}

void Engine::MakeContextCurrent() const
{
	if (this->m_glcontext_canvas == nullptr)
	{
		bool context_set = false;
		for (RenderController* render_controller : this->m_render_controllers)
		{
			if (!context_set)
			{
				if (render_controller->GetType() == RenderControllerType::EngineCanvas)
				{
					EngineCanvasController* engine_canvas_controller = dynamic_cast<EngineCanvasController*>(render_controller);
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

bool operator==(const Engine::LoadedGeometry& first, const Engine::LoadedGeometry& second)
{
	if (first.data != second.data)
	{
		return false;
	}

	if (first.vbo != second.vbo)
	{
		return false;
	}

	return true;
}

bool operator!=(const Engine::LoadedGeometry& first, const Engine::LoadedGeometry& second)
{
	return !(first == second);
}

void LogMessage(std::string message, bool show_time)
{
	std::ofstream output_file;
	output_file.open(GAMEENGINE_LOG_PATH, std::ios_base::app);

	std::string padding;
	if (show_time)
	{
		std::chrono::system_clock::time_point now_time_point = std::chrono::system_clock::now();
		std::time_t now_time_t = std::chrono::system_clock::to_time_t(now_time_point);
		std::string now_string = std::ctime(&now_time_t);
		now_string = now_string.substr(0, now_string.size() - 1); //remove the newline that is added for some reason

		output_file << now_string << ": ";

		for (std::size_t i = 0; i < now_string.size() + 2U; i++)
		{
			padding += ' ';
		}
	}

	std::vector<std::string> lines = { "" };
	for (char& character : message)
	{
		if (character == '\n')
		{
			if (!lines.back().empty())
			{
				lines.push_back("");
			}
		}
		else
		{
			lines.back() += character;
		}
	}

	for (std::size_t i = 0U; i < lines.size(); i++)
	{
		std::string& line = lines.at(i);

		if (!(
			i != 0U
			&& i + 1U == lines.size()
			&& line.empty()
			))
		{
			if (i != 0U)
			{
				output_file << padding;
			}

			output_file << line << std::endl;
		}
	}

	output_file.close();
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
	case GL_DEBUG_TYPE_PUSH_GROUP: err_type = "push group"; break;
	case GL_DEBUG_TYPE_POP_GROUP: err_type = "pop group"; break;

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
		|| type == GL_CONTEXT_LOST)
	{
		throw std::runtime_error(message);
	}
}

void Engine::LoadedGeometry::FreeGL() const
{
	glDeleteBuffers(1, &this->vbo);
	glDeleteVertexArrays(1, &this->vao);
}
