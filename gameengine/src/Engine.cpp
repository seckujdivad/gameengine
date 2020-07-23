#include <wx/wxprec.h>
#include "Engine.h"

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

Engine::Engine(wxWindow* parent)
{
	this->m_parent = parent;
	
	this->m_canvas_args.PlatformDefaults().Depth(24).Stencil(8).RGBA().DoubleBuffer().EndList();

	wxGLCanvas* temp_canvas = new wxGLCanvas(this->m_parent, this->m_canvas_args, wxID_ANY);
	wxGLContextAttrs ctx_attrs;
	ctx_attrs.PlatformDefaults().CoreProfile().MajorVersion(4).MinorVersion(3).EndList();
	this->m_glcontext = new wxGLContext(temp_canvas, NULL, &ctx_attrs);

	temp_canvas->SetCurrent(*this->m_glcontext);

	std::remove(ENGINECANVAS_LOG_PATH);

	glewExperimental = GL_TRUE;
	if (glewInit() != GLEW_OK)
	{
		throw std::runtime_error("Couldn't initialise glew");
	}

	glLoadIdentity();

	glEnable(GL_CULL_FACE);
	glEnable(GL_DEPTH_TEST);

	glEnable(GL_DEBUG_OUTPUT);
	glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
	glDebugMessageCallback(MessageCallback, 0);

	delete temp_canvas;
}

Engine::~Engine()
{
	delete this->m_glcontext;
}

EngineCanvas* Engine::GenerateNewCanvas(std::vector<std::tuple<std::string, GLenum>> shaders, wxWindowID id, wxWindow* parent)
{
	EngineCanvas* canvas = new EngineCanvas(parent == nullptr ? this->m_parent : parent, id, this->m_canvas_args, this->m_glcontext, this, shaders);
	canvas->MakeOpenGLFocus();
	this->m_render_outputs.push_back(canvas);
	return canvas;
}

void Engine::Render()
{
	if (this->m_render_outputs.size() != 0)
	{
		this->m_render_outputs.at(0)->MakeOpenGLFocus();
	}

	if (this->m_scene != nullptr)
	{
		//load unloaded static textures
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
		}

		//load unloaded dynamic textures

		//rerender dynamic textures if required
		for (auto it = this->m_textures_rendered.begin(); it != this->m_textures_rendered.end(); it++)
		{
			it->second->Render();
		}

		//rerender engine canvasses
		for (int i = 0; i < this->m_render_outputs.size(); i++)
		{
			this->m_render_outputs.at(i)->Render();
		}

		glFlush();
	}
}

void Engine::SetScene(Scene* scene)
{
	if (this->m_scene != scene)
	{
		this->m_scene = scene;

		//dealloc old scene textures
		for (auto it = this->m_textures_rendered.begin(); it != this->m_textures_rendered.end(); it++)
		{
			delete it->second;
		}
		this->m_textures_rendered.clear();

		for (auto it = this->m_textures_static.begin(); it != this->m_textures_static.end(); it++)
		{
			glDeleteTextures(1, &it->second.id);
		}
		this->m_textures_static.clear();

		for (auto it = this->m_textures_cubemap.begin(); it != this->m_textures_cubemap.end(); it++)
		{
			delete it->second;
		}
		this->m_textures_cubemap.clear();
	}
}

Scene* Engine::GetScene()
{
	return this->m_scene;
}

LoadedTexture Engine::GetTexture(TextureReference reference)
{
	return LoadedTexture();
}
