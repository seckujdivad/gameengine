#pragma once

#include <unordered_set>
#include <vector>
#include <memory>

#include "RenderController.h"
#include "../RenderMode.h"
#include "../rendertarget/target/RenderTargetConfig.h"
#include "../renderer/Renderer.h"

#include <glm/glm.hpp>

class EngineCanvas;
class RenderTexture;
class Engine;

class EngineCanvasController : public RenderController
{
public:
	struct CompositeLayer
	{
		RenderMode mode = RenderMode::Normal;
		glm::vec4 colour_translate = glm::vec4(0.0f);
		glm::vec4 colour_scale = glm::vec4(1.0f);
	};

private:
	EngineCanvas* m_canvas;

	std::vector<std::unique_ptr<Renderer>> m_renderers;
	std::vector<std::unique_ptr<RenderTexture>> m_textures;
	std::unique_ptr<RenderTexture> m_texture_final;

	RenderTargetConfig RemakeTextures(std::vector<EngineCanvasController::CompositeLayer> composite_layers);

public:
	EngineCanvasController(Engine* engine, RenderTextureReference reference, EngineCanvas* canvas, std::vector<CompositeLayer> composites);

	void Render(bool continuous_draw) override;
	std::shared_ptr<RenderTextureGroup> GetRenderTexture() const override;
	RenderControllerType GetType() const override;

	EngineCanvas* GetEngineCanvas() const;

	void SetRenderLayers(std::vector<EngineCanvasController::CompositeLayer> composite_layers);
	void SetRenderLayers(std::vector<RenderMode> modes);
	void SetRenderLayers(RenderMode mode);

	std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const override;
	bool IsEssentialDraw() const override;
};