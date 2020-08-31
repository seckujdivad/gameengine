#include "Engine.h"

#include <tuple>
#include <wx/image.h>
#include <algorithm>

#include "scene/Scene.h"

#include "render/Renderable.h"
#include "render/EngineCanvas.h"

#include "render/controllers/RenderController.h"
#include "render/controllers/EngineCanvasController.h"
#include "render/controllers/ShadowController.h"
#include "render/controllers/SkyboxController.h"
#include "render/controllers/ReflectionController.h"

void Engine::LoadTexture(LocalTexture texture, std::string uniform_name)
{
	GLuint texture_id;
	glGenTextures(1, &texture_id);
	glBindTexture(GL_TEXTURE_2D, texture_id);

	//wrapping
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

	//filter
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, texture.GetMinFilter() == LocalTextureFilter::Nearest ? GL_NEAREST : GL_LINEAR); //shrinking filter
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, texture.GetMagFilter() == LocalTextureFilter::Nearest ? GL_NEAREST : GL_LINEAR); //enlarging filter

	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, std::get<0>(texture.GetDimensions()), std::get<1>(texture.GetDimensions()), 0, GL_RGB, GL_UNSIGNED_BYTE, texture.GetData());
	glGenerateMipmap(GL_TEXTURE_2D);

	LoadedTexture texture_data;
	texture_data.id = texture_id;
	texture_data.type = GL_TEXTURE_2D;
	texture_data.uniform_name = uniform_name;

	this->m_textures_static.insert({ texture.GetReference(), texture_data });
}

Engine::LoadedGeometry Engine::LoadGeometry(const ModelGeometry& geometry)
{
	LoadedGeometry loaded_geometry;

	std::vector<double> vertices_highp = GetTriangles(geometry);
	std::vector<GLfloat> vertices = DoubleToSinglePrecision(vertices_highp);

	loaded_geometry.num_vertices = vertices.size() / Model::GetValuesPerVert();
	loaded_geometry.geometry = geometry;

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

Engine::Engine(wxWindow* parent, Scene* scene) : m_scene(scene), m_parent(parent)
{
	this->m_canvas_args.PlatformDefaults().Depth(24).Stencil(8).RGBA().DoubleBuffer().EndList();

	if (!wxGLCanvas::IsDisplaySupported(this->m_canvas_args))
	{
		throw std::runtime_error("Display settings are not supported");
	}

	this->m_glcontext_canvas = new wxGLCanvas(parent, this->m_canvas_args, wxID_ANY);

	wxGLContextAttrs ctx_attrs;
	ctx_attrs.PlatformDefaults().CoreProfile().MajorVersion(4).MinorVersion(3).EndList();
	this->m_glcontext = new wxGLContext(this->m_glcontext_canvas, NULL, &ctx_attrs);

	if (!this->m_glcontext->IsOK())
	{
		throw std::runtime_error("OpenGL Context is not correct");
	}

	this->m_glcontext_canvas->SetCurrent(*this->m_glcontext);

	std::remove(ENGINECANVAS_LOG_PATH);

	glewExperimental = GL_TRUE;
	if (glewInit() != GLEW_OK)
	{
		throw std::runtime_error("Couldn't initialise glew");
	}

	glLoadIdentity();

	glEnable(GL_DEBUG_OUTPUT);
	glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
	glDebugMessageCallback(MessageCallback, 0);
}

Engine::~Engine()
{
	for (RenderController* render_controller : this->m_render_controllers)
	{
		delete render_controller;
	}

	for (auto [reference, loaded_texture] : this->m_textures_static)
	{
		glDeleteTextures(1, &loaded_texture.id);
	}

	for (auto [reference, loaded_geometry] : this->m_model_geometry_vbos)
	{
		glDeleteBuffers(1, &loaded_geometry.vbo);
		glDeleteVertexArrays(1, &loaded_geometry.vao);
	}

	for (auto [model, loaded_geometry] : this->m_temporary_vbos)
	{
		glDeleteBuffers(1, &loaded_geometry.vbo);
		glDeleteVertexArrays(1, &loaded_geometry.vao);
	}

	delete this->m_glcontext;
}

EngineCanvas* Engine::GenerateNewCanvas(RenderMode mode, wxWindowID id, wxWindow* parent)
{
	EngineCanvas* canvas = new EngineCanvas(parent == nullptr ? this->m_parent : parent, id, this->m_canvas_args, this->m_glcontext, this, RenderMode::Postprocess);
	canvas->MakeOpenGLFocus();

	EngineCanvasController* controller = new EngineCanvasController(this, this->m_scene->GetNewRenderTextureReference(), canvas, mode); //need a render texture ref for this
	this->m_render_controllers.push_back(controller);

	return canvas;
}

void Engine::Render()
{
	if (this->m_scene != nullptr)
	{
		this->MakeContextCurrent();

		//load unloaded static textures
		// load model textures
		std::vector<Model*> models = this->m_scene->GetModels();
		for (std::vector<Model*>::iterator it = models.begin(); it != models.end(); it++)
		{
			{
				std::map<TextureReference, LoadedTexture>::iterator it2 = this->m_textures_static.find((*it)->GetColourTexture().GetReference());
				if (it2 == this->m_textures_static.end())
				{
					this->LoadTexture((*it)->GetColourTexture(), "colourTexture");
				}
			}

			{
				std::map<TextureReference, LoadedTexture>::iterator it2 = this->m_textures_static.find((*it)->GetNormalTexture().GetReference());
				if (it2 == this->m_textures_static.end())
				{
					this->LoadTexture((*it)->GetNormalTexture(), "normalTexture");
				}
			}

			{
				std::map<TextureReference, LoadedTexture>::iterator it2 = this->m_textures_static.find((*it)->GetSpecularTexture().GetReference());
				if (it2 == this->m_textures_static.end())
				{
					this->LoadTexture((*it)->GetSpecularTexture(), "specularTexture");
				}
			}

			{
				std::map<TextureReference, LoadedTexture>::iterator it2 = this->m_textures_static.find((*it)->GetReflectionTexture().GetReference());
				if (it2 == this->m_textures_static.end())
				{
					this->LoadTexture((*it)->GetReflectionTexture(), "reflectionIntensityTexture");
				}
			}

			{
				std::map<TextureReference, LoadedTexture>::iterator it2 = this->m_textures_static.find((*it)->GetSkyboxMaskTexture().GetReference());
				if (it2 == this->m_textures_static.end())
				{
					this->LoadTexture((*it)->GetSkyboxMaskTexture(), "skyboxMaskTexture");
				}
			}
		}

		//load required cubemaps and unload unused ones
		std::vector<std::tuple<RenderTextureReference, CubemapType>> cubemaps_to_add;
		std::vector<std::tuple<RenderTextureReference, CubemapType>> cubemaps_to_remove;
		{
			//perform diff for cubemap controllers
			std::vector<std::tuple<RenderTextureReference, CubemapType>> existing_cubemaps;
			std::vector<std::tuple<RenderTextureReference, CubemapType>> required_cubemaps;

			for (int i = 0; i < (int)this->m_render_controllers.size(); i++)
			{
				if (this->m_render_controllers.at(i)->GetType() == RenderControllerType::Reflection)
				{
					existing_cubemaps.push_back({ this->m_render_controllers.at(i)->GetReference(), CubemapType::Reflection });
				}
				else if (this->m_render_controllers.at(i)->GetType() == RenderControllerType::Shadow)
				{
					existing_cubemaps.push_back({ this->m_render_controllers.at(i)->GetReference(), CubemapType::Pointlight });
				}
				else if (this->m_render_controllers.at(i)->GetType() == RenderControllerType::Skybox)
				{
					existing_cubemaps.push_back({ this->m_render_controllers.at(i)->GetReference(), CubemapType::Skybox });
				}
			}

			std::vector<std::tuple<Cubemap*, CubemapType>> required_cubemap_ptrs = this->m_scene->GetCubemaps();
			for (int i = 0; i < (int)required_cubemap_ptrs.size(); i++)
			{
				required_cubemaps.push_back({ std::get<0>(required_cubemap_ptrs.at(i))->GetReference(), std::get<1>(required_cubemap_ptrs.at(i)) });
			}

			if (this->m_scene->GetSkyboxScene() != nullptr)
			{
				required_cubemaps.push_back({ this->m_scene->GetSkyboxTextureReference(), CubemapType::Skybox });
			}

			std::sort(existing_cubemaps.begin(), existing_cubemaps.end());
			std::sort(required_cubemaps.begin(), required_cubemaps.end());

			enum class State
			{
				Both,
				Added,
				Removed
			};

			int i = 0;
			int j = 0;
			while ((i < (int)existing_cubemaps.size()) || (j < (int)required_cubemaps.size()))
			{
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
			for (int i = 0; i < (int)cubemaps_to_remove.size(); i++)
			{
				for (int j = 0; j < (int)this->m_render_controllers.size(); j++)
				{
					if (this->m_render_controllers.at(j)->GetReference() == std::get<0>(cubemaps_to_remove.at(i)))
					{
						cubemap_indices.push_back(j);
					}
				}
			}

			std::sort(cubemap_indices.begin(), cubemap_indices.end(), [](int first, int second) {return first > second; }); //order high-low

			for (int i = 0; i < (int)cubemap_indices.size(); i++)
			{
				delete this->m_render_controllers.at(i);
				this->m_render_controllers.erase(this->m_render_controllers.begin() + i);
			}
		}

		//create new cubemap controllers
		for (std::tuple<RenderTextureReference, CubemapType> cubemap_to_add : cubemaps_to_add)
		{
			RenderTextureReference reference = std::get<0>(cubemap_to_add);
			CubemapType type = std::get<1>(cubemap_to_add);

			if (type == CubemapType::Reflection)
			{
				this->m_render_controllers.push_back(new ReflectionController(this, reference));
			}
			else if (type == CubemapType::Pointlight)
			{
				this->m_render_controllers.push_back(new ShadowController(this, reference));
			}
			else if (type == CubemapType::Skybox)
			{
				this->m_render_controllers.push_back(new SkyboxController(this, reference));
			}
			else
			{
				throw std::runtime_error("Invalid CubemapType enum: " + std::to_string((int)type));
			}
		}

		//remove non-existent geometry and update existing (if required)
		{
			std::vector<std::tuple<ModelReference, LoadedGeometry>> to_remove;
			for (auto it = this->m_model_geometry_vbos.begin(); it != this->m_model_geometry_vbos.end(); it++)
			{
				Model* model = this->GetScene()->GetModel(it->first);

				if (model == nullptr)
				{
					to_remove.push_back(std::tuple(it->first, it->second));
				}
				else
				{
					ModelGeometry geometry = it->second.geometry;
					if (geometry != model->GetGeometry())
					{
						std::vector<GLfloat> vertices = DoubleToSinglePrecision(model->GetTriangles());

						glBindVertexArray(it->second.vao);
						glBindBuffer(GL_ARRAY_BUFFER, it->second.vbo);
						glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat)* vertices.size(), vertices.data(), GL_DYNAMIC_DRAW);

						it->second.geometry = model->GetGeometry();
						it->second.num_vertices = vertices.size() / model->GetValuesPerVert();
					}
				}
			}

			for (int i = 0; i < (int)to_remove.size(); i++)
			{
				glDeleteBuffers(1, &std::get<1>(to_remove.at(i)).vbo);
				this->m_model_geometry_vbos.erase(std::get<0>(to_remove.at(i)));
			}

			for (Model* model : this->GetScene()->GetModels())
			{
				if (this->m_model_geometry_vbos.count(model->GetReference()) == 0)
				{
					this->m_model_geometry_vbos.insert(std::pair(model->GetReference(), this->LoadGeometry(model->GetGeometry())));
				}
			}
		}

		//tell controllers to redraw themselves (if required)
		std::sort(this->m_render_controllers.begin(), this->m_render_controllers.end(), [](RenderController* first, RenderController* second) {return first->GetRenderGroup() < second->GetRenderGroup();}); //lower render groups are rendered first

		for (RenderController* render_controller : this->m_render_controllers)
		{
			if (render_controller->GetType() != RenderControllerType::Skybox) //skybox rendering is not yet supported
			{
				render_controller->Render();
			}
		}
	}
}

Scene* Engine::GetScene()
{
	return this->m_scene;
}

LoadedTexture Engine::GetTexture(TextureReference reference)
{
	return this->m_textures_static.at(reference);
}

RenderTextureGroup Engine::GetRenderTexture(RenderTextureReference reference)
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

Engine::LoadedGeometry Engine::BindVAO(Model* model)
{
	LoadedGeometry loaded_geometry;
	if (this->m_model_geometry_vbos.count(model->GetReference()) == 0) //generate temporary VBO
	{
		loaded_geometry = this->LoadGeometry(model->GetGeometry());
		this->m_temporary_vbos.insert(std::pair(model, loaded_geometry));
	}
	else
	{
		loaded_geometry = this->m_model_geometry_vbos.at(model->GetReference());
	}

	glBindVertexArray(loaded_geometry.vao);
	glBindBuffer(GL_ARRAY_BUFFER, loaded_geometry.vbo);

	return loaded_geometry;
}

void Engine::ReleaseVAO(Model* model)
{
	if (this->m_temporary_vbos.count(model) == 0)
	{
		throw std::runtime_error("This model has no associated temporary VBO");
	}
	else
	{
		glDeleteBuffers(1, &this->m_temporary_vbos.at(model).vbo);
		glDeleteVertexArrays(1, &this->m_temporary_vbos.at(model).vao);
		this->m_temporary_vbos.erase(model);
	}
}

void Engine::MakeContextCurrent()
{
	this->m_glcontext_canvas->SetCurrent(*this->m_glcontext);

	for (RenderController*& controller : this->m_render_controllers)
	{
		if (controller->GetType() == RenderControllerType::EngineCanvas)
		{
			EngineCanvasController* canvas_controller = dynamic_cast<EngineCanvasController*>(controller);
			canvas_controller->GetEngineCanvas()->MakeOpenGLFocus();
		}
	}
}

bool operator==(const Engine::LoadedGeometry& first, const Engine::LoadedGeometry& second)
{
	if (first.geometry != second.geometry)
	{
		return false;
	}

	if (first.num_vertices != second.num_vertices)
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
