#pragma once

#include "RenderJobFactory.h"
#include "../rendertarget/RenderTargetConfig.h"

class RenderTexture;
class Camera;

struct NormalRenderJobInitialiser : public RenderJobInitialiser
{
};

class NormalRenderJobFactory : public RenderJobFactory
{
private:
	std::unique_ptr<NormalRenderJobInitialiser> m_default_initialiser;

	std::unique_ptr<RenderTexture> m_rendertexture_first_pass;
	std::unique_ptr<RenderTexture> m_rendertexture_pointlight;

	Camera* m_camera = nullptr;

public:
	NormalRenderJobFactory(Engine* engine, RenderTarget* target);

	std::shared_ptr<RenderJob> GenerateJob(RenderJobInitialiser* initialiser) override;
	bool SetOutputSize(std::tuple<int, int> dimensions) override;
	void CopyFrom(const RenderJobFactory* src) const override;
	std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const override;

	NormalRenderJobInitialiser& GetDefaultInitialiser();

	bool SetDrawReflections(bool value);
	bool SetDrawShadows(bool value);

	//do not call directly
	void Render(std::vector<Model*> models, bool continuous_draw = false);
};