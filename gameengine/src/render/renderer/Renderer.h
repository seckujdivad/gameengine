#pragma once

#include <vector>
#include <tuple>
#include <unordered_set>

#include "../../scene/Referenceable.h"

class Engine;
class RenderTarget;
class Camera;
class Model;

class Renderer
{
private:
	Engine* m_engine;
	RenderTarget* m_target;

public:
	Renderer(Engine* engine, RenderTarget* target);
	virtual ~Renderer();

	Engine* GetEngine() const;
	RenderTarget* GetTarget() const;

	virtual Camera* GetCamera() const;
	virtual void SetCamera(Camera* camera);

	std::tuple<int, int> GetOutputSize() const;
	virtual bool SetOutputSize(std::tuple<int, int> dimensions) = 0;

	virtual void CopyFrom(const Renderer* src) const = 0;
	void CopyTo(const Renderer* dest) const;

	virtual std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const = 0;

	virtual void Render(std::vector<Model*> models, bool continuous_draw = false) = 0;
};