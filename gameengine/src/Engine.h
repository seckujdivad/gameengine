#pragma once

#include <nlohmann/json.hpp>

#include <wx/wxprec.h>
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
#include "render/EngineCanvas.h"
#include "Resource.h"
#include "scene/Referenceable.h"
#include "render/RenderTexture.h"
#include "render/ShaderProgram.h"
#include "scene/Scene.h"
#include "render/controllers/RenderController.h"
#include "render/controllers/EngineCanvasController.h"
#include "render/controllers/ShadowController.h"

class Engine
{
private:
	wxGLContext* m_glcontext;
	wxWindow* m_parent;
	wxGLAttributes m_canvas_args;

	Scene* m_scene = nullptr;

	std::map<TextureReference, LoadedTexture> m_textures_static;

	std::vector<RenderController*> m_render_controllers;

	void LoadTexture(LocalTexture texture, std::string uniform_name);

public:
	Engine(wxWindow* parent, Scene* scene);
	Engine(const Engine&) = delete;
	Engine& operator=(const Engine&) = delete;
	~Engine();

	EngineCanvas* GenerateNewCanvas(RenderMode mode, wxWindowID id = wxID_ANY, wxWindow* parent = nullptr);

	void Render();

	Scene* GetScene();

	LoadedTexture GetTexture(TextureReference reference);
	RenderTextureGroup GetRenderTexture(RenderTextureReference reference);
};
