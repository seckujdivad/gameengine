#pragma once

#ifdef _DEBUG
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#endif

#include <string>
#include <vector>
#include <map>

#include "GLComponents.h"

#include "scene/Referenceable.h"
#include "scene/model/Model.h"
#include "render/LoadedTexture.h"
#include "render/RenderTextureData.h"
#include "render/RenderMode.h"
#include "render/controllers/EngineCanvasController.h"

class EngineCanvas;
class Scene;
class Renderable;

class Engine
{
public:
	struct LoadedGeometry
	{
		ModelGeometry geometry;
		GLuint vao = NULL;
		GLuint vbo = NULL;
		int num_vertices = 0;
	};

	struct DebugMessageConfig
	{
		GLenum source = GL_DONT_CARE;
		GLenum type = GL_DONT_CARE;
		GLenum severity = GL_DONT_CARE;
		bool enabled = true;
	};

private:
	wxGLContext* m_glcontext;
	wxWindow* m_parent;
	wxGLAttributes m_canvas_args;
	wxGLCanvas* m_glcontext_canvas;

	Scene* m_scene = nullptr;

	std::map<TextureReference, LoadedTexture> m_textures_static;

	std::vector<RenderController*> m_render_controllers;

	//loaded geometry
	std::map<ModelReference, Engine::LoadedGeometry> m_model_geometry_vbos;
	std::map<Model*, Engine::LoadedGeometry> m_temporary_vbos;

	Engine::LoadedGeometry LoadGeometry(const ModelGeometry& geometry);

	void AddRenderController(RenderController* render_controller);

public:
	Engine(wxWindow* parent, Scene* scene);
	Engine(const Engine&) = delete;
	Engine& operator=(const Engine&) = delete;
	Engine(Engine&&) = delete;
	Engine& operator=(Engine&&) = delete;
	~Engine();

	EngineCanvas* GenerateNewCanvas(std::vector<EngineCanvasController::CompositeLayer> composite_layers, wxWindowID id = wxID_ANY, wxWindow* parent = nullptr);
	EngineCanvas* GenerateNewCanvas(std::vector<RenderMode> modes, wxWindowID id = wxID_ANY, wxWindow* parent = nullptr);
	EngineCanvas* GenerateNewCanvas(RenderMode mode, wxWindowID id = wxID_ANY, wxWindow* parent = nullptr);

	void Render();

	Scene* GetScene() const;

	void LoadTexture(LocalTexture texture, std::string uniform_name);
	LoadedTexture GetTexture(TextureReference reference) const;
	RenderTextureGroup GetRenderTexture(RenderTextureReference reference) const;

	Engine::LoadedGeometry BindVAO(Model* model);
	void ReleaseVAO(Model* model);

	void MakeContextCurrent() const;

	void SetDebugMessageLevel(Engine::DebugMessageConfig config) const;
	void SetDebugMessageLevel(std::vector<Engine::DebugMessageConfig> config) const;
};

bool operator==(const Engine::LoadedGeometry& first, const Engine::LoadedGeometry& second);
bool operator!=(const Engine::LoadedGeometry& first, const Engine::LoadedGeometry& second);

void LogMessage(std::string message, bool show_time = true);