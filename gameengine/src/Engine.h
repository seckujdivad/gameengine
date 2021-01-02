#pragma once

#ifdef _DEBUG
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#endif

#include <string>
#include <vector>
#include <unordered_map>
#include <memory>
#include <tuple>
#include <functional>

#include "GLComponents.h"

#include "scene/Referenceable.h"
#include "scene/model/Model.h"
#include "render/LoadedTexture.h"
#include "render/RenderTextureData.h"
#include "render/controllers/EngineCanvasController.h"
#include "scene/SceneChild.h"
#include "scene/model/geometry/PresetGeometry.h"

class EngineCanvas;
class Scene;
class Renderable;

class Engine : public SceneChild
{
public:
	struct LoadedGeometry
	{
		std::vector<GLfloat> data;

		GLsizei buffer_len = 0;
		GLuint vao = NULL;
		GLuint vbo = NULL;

		void FreeGL() const;
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

	bool m_single_context_mode = false;

	std::unordered_map<TextureReference, std::tuple<LoadedTexture, LocalTexture>> m_textures_static;

	std::vector<RenderController*> m_render_controllers;

	//loaded geometry
	std::unordered_map<ModelReference, std::unordered_map<Geometry::RenderInfo, Engine::LoadedGeometry, Geometry::RenderInfo::Hash>> m_model_geometry;
	std::unordered_map<PresetGeometry::GeometryType, std::tuple<Geometry::RenderInfo, Engine::LoadedGeometry>> m_geometry_presets;

	std::unordered_map<Geometry::RenderInfo, std::vector<GLfloat>, Geometry::RenderInfo::Hash> GenerateGeometryGroups(std::vector<std::shared_ptr<Geometry>> geometry);
	Engine::LoadedGeometry CreateLoadedGeometry(std::vector<GLfloat> vertices, std::size_t primitive_size, Geometry::PrimitiveType primitive_type);
	std::unordered_map<Geometry::RenderInfo, Engine::LoadedGeometry, Geometry::RenderInfo::Hash> LoadGeometry(std::vector<std::shared_ptr<Geometry>> geometry);

	void BindVAO(Engine::LoadedGeometry loaded_geometry);

	void PrunePresetGeometry(PresetGeometry::GeometryType type);

	void AddRenderController(RenderController* render_controller);
	RenderController* GetRenderController(RenderTextureReference reference);

	std::vector<RenderTextureReference> CollateRenderTextureDependencies(RenderTextureReference reference, std::unordered_map<RenderTextureReference, std::unordered_set<RenderTextureReference>>& direct_dependencies, std::unordered_map<RenderTextureReference, bool>& is_drawn);

public:
	Engine(wxWindow* parent, Scene* scene, bool single_context_mode = false);
	Engine(const Engine&) = delete;
	Engine& operator=(const Engine&) = delete;
	Engine(Engine&&) = delete;
	Engine& operator=(Engine&&) = delete;
	~Engine();

	EngineCanvasController* GenerateNewCanvas(std::vector<EngineCanvasController::CompositeLayer> composite_layers, wxWindowID id = wxID_ANY, wxWindow* parent = nullptr);
	EngineCanvasController* GenerateNewCanvas(std::vector<RenderableConfig> configs, wxWindowID id = wxID_ANY, wxWindow* parent = nullptr);
	EngineCanvasController* GenerateNewCanvas(RenderableConfig config, wxWindowID id = wxID_ANY, wxWindow* parent = nullptr);

	void Render();

	void LoadTexture(LocalTexture texture, std::string uniform_name);
	LoadedTexture GetTexture(TextureReference reference) const;
	RenderTextureGroup GetRenderTexture(RenderTextureReference reference) const;

	void DrawModel(Model* model, std::function<GLenum(Geometry::RenderInfo info, const LoadedGeometry& loaded_geometry)> predraw);

	void MakeContextCurrent(bool force = false) const;

	void SetDebugMessageLevel(Engine::DebugMessageConfig config) const;
	void SetDebugMessageLevel(std::vector<Engine::DebugMessageConfig> config) const;
};

bool operator==(const Engine::LoadedGeometry& first, const Engine::LoadedGeometry& second);
bool operator!=(const Engine::LoadedGeometry& first, const Engine::LoadedGeometry& second);

void LogMessage(std::string message, bool show_time = true);