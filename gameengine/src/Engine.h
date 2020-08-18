#pragma once

#include <nlohmann/json.hpp>

#include <wx/image.h>

#include <string>
#include <vector>
#include <map>
#include <algorithm>

#ifdef _DEBUG
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#endif

#include "GLComponents.h"
#include "Resource.h"

#include "scene/Scene.h"
#include "scene/Referenceable.h"

#include "render/Renderable.h"
#include "render/EngineCanvas.h"
#include "render/RenderTexture.h"
#include "render/ShaderProgram.h"

class RenderController;
class EngineCanvasController;
class ReflectionController;
class ShadowController;
class SkyboxController;
class EngineCanvas;

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

public:
	Engine(wxWindow* parent, Scene* scene);
	Engine(const Engine&) = delete;
	Engine& operator=(const Engine&) = delete;
	~Engine();

	EngineCanvas* GenerateNewCanvas(RenderMode mode, wxWindowID id = wxID_ANY, wxWindow* parent = nullptr);

	void Render();

	Scene* GetScene();

	void LoadTexture(LocalTexture texture, std::string uniform_name);
	LoadedTexture GetTexture(TextureReference reference);
	RenderTextureGroup GetRenderTexture(RenderTextureReference reference);

	Engine::LoadedGeometry BindVAO(Model* model);
	void ReleaseVAO(Model* model);

	void MakeContextCurrent();
};

bool operator==(const Engine::LoadedGeometry& first, const Engine::LoadedGeometry& second);
bool operator!=(const Engine::LoadedGeometry& first, const Engine::LoadedGeometry& second);

#include "render/controllers/RenderController.h"
#include "render/controllers/EngineCanvasController.h"
#include "render/controllers/ShadowController.h"
#include "render/controllers/SkyboxController.h"
#include "render/controllers/ReflectionController.h"