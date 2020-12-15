#include "Engine.h"

#include <tuple>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <chrono>
#include <optional>

#include <wx/image.h>

#include "scene/Scene.h"

#include "render/Renderable.h"
#include "render/EngineCanvas.h"

#include "render/controllers/RenderController.h"
#include "render/controllers/ShadowController.h"
#include "render/controllers/SkyboxController.h"
#include "render/controllers/ReflectionController.h"

const char GAMEENGINE_LOG_PATH[] = "gameengine_GL.log";

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

Engine::LoadedGeometry Engine::LoadGeometry(std::shared_ptr<Geometry> geometry)
{
	LoadedGeometry loaded_geometry(geometry);

	loaded_geometry.data = geometry->GetPrimitives();
	std::vector<GLfloat> vertices;
	vertices.reserve(loaded_geometry.data.size());
	for (double value : loaded_geometry.data)
	{
		vertices.push_back(static_cast<GLfloat>(value));
	}

	//create vbo
	glGenVertexArrays(1, &loaded_geometry.vao);
	glBindVertexArray(loaded_geometry.vao);

	glGenBuffers(1, &loaded_geometry.vbo);
	glBindBuffer(GL_ARRAY_BUFFER, loaded_geometry.vbo);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * vertices.size(), vertices.data(), GL_DYNAMIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 14 * sizeof(GLfloat), 0);
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 14 * sizeof(GLfloat), (void*)(3 * sizeof(GLfloat)));
	glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 14 * sizeof(GLfloat), (void*)(6 * sizeof(GLfloat)));
	glVertexAttribPointer(3, 3, GL_FLOAT, GL_FALSE, 14 * sizeof(GLfloat), (void*)(8 * sizeof(GLfloat)));
	glVertexAttribPointer(4, 3, GL_FLOAT, GL_FALSE, 14 * sizeof(GLfloat), (void*)(11 * sizeof(GLfloat)));

	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glEnableVertexAttribArray(2);
	glEnableVertexAttribArray(3);
	glEnableVertexAttribArray(4);

	return loaded_geometry;
}

void Engine::AddRenderController(RenderController* render_controller)
{
	int insert_index = 0;
	for (int i = 0; i < static_cast<int>(this->m_render_controllers.size()); i++)
	{
		if (this->m_render_controllers.at(i)->GetRenderGroup() <= render_controller->GetRenderGroup())
		{
			insert_index = i;
		}
	}

	this->m_render_controllers.insert(this->m_render_controllers.begin() + insert_index, render_controller);
}

Engine::Engine(wxWindow* parent, Scene* scene) : m_scene(scene), m_parent(parent)
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

	for (const auto& [reference, loaded_geometries] : this->m_model_geometry_vbos)
	{
		for (const LoadedGeometry& loaded_geometry : loaded_geometries)
		{
			glDeleteBuffers(1, &loaded_geometry.vbo);
			glDeleteVertexArrays(1, &loaded_geometry.vao);
		}
	}

	for (const auto& [model, loaded_geometries] : this->m_temporary_vbos)
	{
		for (const LoadedGeometry& loaded_geometry : loaded_geometries)
		{
			glDeleteBuffers(1, &loaded_geometry.vbo);
			glDeleteVertexArrays(1, &loaded_geometry.vao);
		}
	}

	delete this->m_glcontext;
}

EngineCanvas* Engine::GenerateNewCanvas(std::vector<EngineCanvasController::CompositeLayer> composite_layers, wxWindowID id, wxWindow* parent)
{
	RenderableConfig empty_config; //configuration of the EngineCanvas is done by the EngineCanvasController
	EngineCanvas* canvas = new EngineCanvas(parent == nullptr ? this->m_parent : parent, id, this->m_canvas_args, this->m_glcontext, this, empty_config);
	canvas->MakeOpenGLFocus();

	EngineCanvasController* controller = new EngineCanvasController(this, this->m_scene->GetNewRenderTextureReference(), canvas, composite_layers);
	this->AddRenderController(controller);

	if (this->m_glcontext_canvas != nullptr)
	{
		this->m_glcontext_canvas->Destroy();
		this->m_glcontext_canvas = nullptr;
	}

	return canvas;
}

EngineCanvas* Engine::GenerateNewCanvas(std::vector<RenderableConfig> configs, wxWindowID id, wxWindow* parent)
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

EngineCanvas* Engine::GenerateNewCanvas(RenderableConfig config, wxWindowID id, wxWindow* parent)
{
	return this->GenerateNewCanvas(std::vector({ config }), id, parent);
}

void Engine::Render()
{
	if (this->m_scene != nullptr)
	{
		this->MakeContextCurrent();

		//update static textures
		// load model textures
		for (Model* model : this->m_scene->GetModels())
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

			std::vector<std::tuple<Cubemap*, CubemapType>> required_cubemap_ptrs = this->m_scene->GetCubemaps();
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
			std::vector<std::tuple<ModelReference, std::vector<LoadedGeometry>>> to_remove;
			for (auto& [model_reference, loaded_geometries] : this->m_model_geometry_vbos)
			{
				Model* model = this->GetScene()->GetModel(model_reference);

				if (model == nullptr)
				{
					to_remove.push_back(std::tuple(model_reference, loaded_geometries));
				}
				else
				{
					std::vector<std::vector<double>> model_geometries;
					for (std::shared_ptr<Geometry> geometry : model->GetGeometry())
					{
						model_geometries.push_back(geometry->GetPrimitives());
					}

					std::vector<std::vector<double>> old_geometries;
					old_geometries.reserve(loaded_geometries.size());
					for (const LoadedGeometry& loaded_geometry : loaded_geometries)
					{
						old_geometries.push_back(loaded_geometry.data);
					}

					if (model_geometries != old_geometries) //check if geometry actually has changed between iterations
					{
						if (model_geometries.size() == loaded_geometries.size()) //changes to geometry are small, new geometry can be loaded into old arrays
						{
							for (int i = 0; i < static_cast<int>(loaded_geometries.size()); i++)
							{
								LoadedGeometry& loaded_geometry = loaded_geometries.at(i);
								std::vector<double>& new_values = model_geometries.at(i);

								std::vector<GLfloat> vertices;
								vertices.reserve(new_values.size());
								for (double value : new_values)
								{
									vertices.push_back(static_cast<GLfloat>(value)); 
								}

								glBindVertexArray(loaded_geometry.vao);
								glBindBuffer(GL_ARRAY_BUFFER, loaded_geometry.vbo);
								glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * vertices.size(), vertices.data(), GL_DYNAMIC_DRAW);

								loaded_geometry.data = new_values;
							}
						}
						else //changes are too large, remove and start again from scratch
						{
							to_remove.push_back(std::tuple(model_reference, loaded_geometries));
						}
					}
				}
			}

			for (const auto& [model_reference, loaded_geometries] : to_remove)
			{
				for (const LoadedGeometry& loaded_geometry : loaded_geometries)
				{
					glDeleteBuffers(1, &loaded_geometry.vbo);
				}
				
				this->m_model_geometry_vbos.erase(model_reference);
			}

			for (Model* model : this->GetScene()->GetModels())
			{
				if (this->m_model_geometry_vbos.count(model->GetReference()) == 0)
				{
					std::vector<LoadedGeometry> loaded_geometries;
					for (std::shared_ptr<Geometry> geometry : model->GetGeometry())
					{
						loaded_geometries.push_back(this->LoadGeometry(geometry));
					}
					this->m_model_geometry_vbos.insert(std::pair(model->GetReference(), loaded_geometries));
				}
			}
		}

		//tell controllers to redraw themselves (if required)
		for (RenderController* render_controller : this->m_render_controllers)
		{
			render_controller->Render();
		}

		for (RenderController* render_controller : this->m_render_controllers)
		{
			render_controller->PostRender();
		}
	}

	glFlush();
}

Scene* Engine::GetScene() const
{
	return this->m_scene;
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

Engine::LoadedGeometry Engine::BindVAO(Model* model, std::shared_ptr<Geometry> geometry)
{
	std::optional<LoadedGeometry> loaded_geometry;
	if (this->m_model_geometry_vbos.count(model->GetReference()) == 0) //generate temporary VBO
	{
		if (this->m_temporary_vbos.count(model) == 0)
		{
			loaded_geometry = this->LoadGeometry(geometry);
			this->m_temporary_vbos.insert(std::pair(model, std::vector({ loaded_geometry.value() })));
		}
		else
		{
			loaded_geometry = this->LoadGeometry(geometry);
			this->m_temporary_vbos.at(model).push_back(loaded_geometry.value());
		}
	}
	else
	{
		const std::vector<LoadedGeometry>& geometries = this->m_model_geometry_vbos.at(model->GetReference());
		for (const LoadedGeometry& geometry_comparison : geometries)
		{
			if (geometry_comparison.source == geometry)
			{
				loaded_geometry = geometry_comparison;
			}
		}
	}

	if (loaded_geometry.has_value())
	{
		glBindVertexArray(loaded_geometry->vao);
		glBindBuffer(GL_ARRAY_BUFFER, loaded_geometry->vbo);

		return loaded_geometry.value();
	}
	else
	{
		throw std::invalid_argument("Provided geometry has not been loaded for this model");
	}
}

void Engine::ReleaseVAOs(Model* model)
{
	if (this->m_temporary_vbos.count(model) == 0)
	{
		throw std::invalid_argument("This model has no associated temporary VBO");
	}
	else
	{
		for (LoadedGeometry& geometry : this->m_temporary_vbos.at(model))
		{
			glDeleteBuffers(1, &geometry.vbo);
			glDeleteVertexArrays(1, &geometry.vao);
		}
		
		this->m_temporary_vbos.erase(model);
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

Engine::LoadedGeometry::LoadedGeometry(std::shared_ptr<Geometry> source) : source(source)
{
}
