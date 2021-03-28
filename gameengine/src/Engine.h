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

#include "render/controllers/EngineCanvasController.h"
#include "render/gltexture/GLTexturePreset.h"
#include "render/glgeometry/GLGeometry.h"

#include "scene/Referenceable.h"
#include "scene/SceneChild.h"
#include "scene/texture/Texture.h"
#include "scene/model/geometry/Geometry.h"
#include "scene/model/geometry/PresetGeometry.h"

class EngineCanvas;
class Scene;
class RenderTarget;
class Model;
class GLTexture;

class Engine : public SceneChild
{
public:
	struct DebugMessageConfig
	{
		GLenum source = GL_DONT_CARE;
		GLenum type = GL_DONT_CARE;
		GLenum severity = GL_DONT_CARE;
		bool enabled = true;
	};

private:
	std::unique_ptr<wxGLContext> m_glcontext;
	wxWindow* m_parent;
	wxGLAttributes m_canvas_args;
	wxGLCanvas* m_glcontext_canvas;

	bool m_single_context_mode = false;

	std::unordered_map<TextureReference, std::tuple<std::shared_ptr<GLTexture>, Texture>> m_textures_static;
	std::unordered_map<GLTexturePreset, std::shared_ptr<GLTexture>, GLTexturePreset::Hash> m_textures_static_presets;

	std::vector<std::unique_ptr<RenderController>> m_render_controllers;

	//loaded geometry
	std::unordered_map<ModelReference, std::unordered_map<Geometry::RenderInfo, GLGeometry, Geometry::RenderInfo::Hash>> m_model_geometry;
	std::unordered_map<PresetGeometry::GeometryType, std::tuple<Geometry::RenderInfo, GLGeometry>> m_geometry_presets;

	std::unordered_map<Geometry::RenderInfo, std::vector<GLfloat>, Geometry::RenderInfo::Hash> GenerateGeometryGroups(std::vector<std::shared_ptr<Geometry>> geometry);
	std::unordered_map<Geometry::RenderInfo, GLGeometry, Geometry::RenderInfo::Hash> LoadGeometry(std::vector<std::shared_ptr<Geometry>> geometry);

	void PrunePresetGeometry(PresetGeometry::GeometryType type);

	void AddRenderController(RenderController* render_controller);
	RenderController* GetRenderController(RenderTextureReference reference) const;

	std::vector<RenderTextureReference> CollateRenderTextureDependencies(RenderTextureReference reference, std::unordered_map<RenderTextureReference, std::unordered_set<RenderTextureReference>>& direct_dependencies, std::unordered_map<RenderTextureReference, bool>& is_drawn);

	void LoadTexture(const Texture& texture);

public:
	Engine(wxWindow* parent, Scene* scene, bool single_context_mode = false);

	EngineCanvasController* GenerateNewCanvas(std::vector<EngineCanvasController::CompositeLayer> composite_layers, wxWindowID id = wxID_ANY, wxWindow* parent = nullptr);
	EngineCanvasController* GenerateNewCanvas(std::vector<RenderMode> modes, wxWindowID id = wxID_ANY, wxWindow* parent = nullptr);
	EngineCanvasController* GenerateNewCanvas(RenderMode mode, wxWindowID id = wxID_ANY, wxWindow* parent = nullptr);

	void Render(bool continuous_draw = false);

	std::shared_ptr<GLTexture> GetTexture(TextureReference reference) const;
	std::shared_ptr<GLTexture> GetTexture(const Texture& texture) const;
	std::shared_ptr<GLTexture> GetTexture(GLTextureDataPreset preset, TargetType target);
	std::shared_ptr<GLTexture> GetTexture(GLTexturePreset preset);
	std::shared_ptr<RenderTextureGroup> GetRenderTexture(RenderTextureReference reference) const;

	void DrawModel(Model* model, std::function<GLenum(Geometry::RenderInfo info, const GLGeometry& loaded_geometry)> predraw);

	void MakeContextCurrent(bool force = false) const;
	bool ContextIsValid() const;

	void SetDebugMessageLevel(Engine::DebugMessageConfig config) const;
	void SetDebugMessageLevel(std::vector<Engine::DebugMessageConfig> config) const;
};