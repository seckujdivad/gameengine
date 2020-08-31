#pragma once

#include <tuple>

#include "RenderController.h"
#include "../RenderMode.h"

class EngineCanvas;
class RenderTexture;
class Engine;

class EngineCanvasController : public RenderController
{
private:
	EngineCanvas* m_canvas;
	RenderTexture* m_texture;

	std::tuple<int, int> m_dimensions_prev = { -1, -1 };

public:
	EngineCanvasController(Engine* engine, RenderTextureReference reference, EngineCanvas* canvas, RenderMode mode);
	EngineCanvasController(const EngineCanvasController& copy_from) = delete;
	EngineCanvasController& operator=(const EngineCanvasController& copy_from) = delete;
	~EngineCanvasController();

	void Render() override;
	RenderTextureGroup GetRenderTexture() const override;
	double GetRenderGroup() const override;
	RenderControllerType GetType() const override;

	EngineCanvas* GetEngineCanvas() const;
};