#pragma once

#include <unordered_set>
#include <vector>
#include <memory>

#include "RenderController.h"
#include "../RenderMode.h"
#include "../rendertarget/RenderTargetConfig.h"
#include "../renderjob/RenderJobFactory.h"

#include <glm/glm.hpp>

class EngineCanvas;
class RenderTexture;
class Engine;

class EngineCanvasController : public RenderController
{
public:
	struct CompositeLayer
	{
		RenderMode mode;
		glm::vec4 colour_translate = glm::vec4(0.0f);
		glm::vec4 colour_scale = glm::vec4(1.0f);
	};

private:
	EngineCanvas* m_canvas;

	std::vector<std::unique_ptr<RenderJobFactory>> m_factories;
	std::vector<std::unique_ptr<RenderTexture>> m_textures;
	std::unique_ptr<RenderTexture> m_texture_final;

	RenderTargetConfig RemakeTextures(std::vector<EngineCanvasController::CompositeLayer> composite_layers);

public:
	EngineCanvasController(Engine* engine, RenderTextureReference reference, EngineCanvas* canvas, std::vector<CompositeLayer> composites);

	void Render() override;
	RenderTextureGroup GetRenderTexture() const override;
	RenderControllerType GetType() const override;

	EngineCanvas* GetEngineCanvas() const;

	void SetRenderLayers(std::vector<EngineCanvasController::CompositeLayer> composite_layers);
	void SetRenderLayers(std::vector<RenderMode> modes);
	void SetRenderLayers(RenderMode mode);

	std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const override;
	bool IsEssentialDraw() const override;
};