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

class Engine
{
private:
	wxGLContext* m_glcontext;
	wxWindow* m_parent;
	wxGLAttributes m_canvas_args;

	Scene* m_scene = nullptr;

	std::map<TextureReference, LoadedTexture> m_textures_static;
	std::vector<RenderTexture*> m_texures_rendered;

	std::map<RenderTextureReference, RenderTexture*> m_textures_rendered;
	std::map<CubemapReference, RenderTexture*> m_textures_cubemap;

	std::vector<EngineCanvas*> m_render_outputs;

	void LoadTexture(LocalTexture texture, std::string uniform_name);

public:
	Engine(wxWindow* parent);
	~Engine();

	EngineCanvas* GenerateNewCanvas(std::vector<std::tuple<std::string, GLenum>> shaders, wxWindowID id = wxID_ANY, wxWindow* parent = nullptr);

	void Render();

	void SetScene(Scene* scene);
	Scene* GetScene();

	LoadedTexture GetTexture(TextureReference reference);
};
