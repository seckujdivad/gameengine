#pragma once

#include <tuple>
#include <vector>

#include "RenderController.h"
#include "../rendertarget/RenderTargetConfig.h"

#include <glm/glm.hpp>

class EngineCanvas;
class RenderTexture;
class Engine;

class EngineCanvasController : public RenderController
{
public:
	struct CompositeLayer
	{
		RenderTargetConfig config;
		glm::vec4 colour_translate = glm::vec4(0.0f);
		glm::vec4 colour_scale = glm::vec4(1.0f);
	};

private:
	EngineCanvas* m_canvas;

	std::vector<RenderTexture*> m_textures;
	RenderTexture* m_texture_final;

	RenderTargetConfig RemakeTextures(std::vector<EngineCanvasController::CompositeLayer> composite_layers);

public:
	EngineCanvasController(Engine* engine, RenderTextureReference reference, EngineCanvas* canvas, std::vector<CompositeLayer> composites);
	EngineCanvasController(const EngineCanvasController&) = delete;
	EngineCanvasController& operator=(const EngineCanvasController&) = delete;
	EngineCanvasController(EngineCanvasController&&) = delete;
	EngineCanvasController& operator=(EngineCanvasController&&) = delete;
	~EngineCanvasController();

	void Render() override;
	RenderTextureGroup GetRenderTexture() const override;
	RenderControllerType GetType() const override;

	EngineCanvas* GetEngineCanvas() const;

	void SetRenderLayers(std::vector<EngineCanvasController::CompositeLayer> composite_layers);
	void SetRenderLayers(std::vector<RenderTargetConfig> configs);
	void SetRenderLayers(RenderTargetConfig config);

	std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const override;
	bool IsEssentialDraw() const override;
};