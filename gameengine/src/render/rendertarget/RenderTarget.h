#pragma once

#include "../../GLComponents.h"

#include <GL/GL.h>

#include <vector>
#include <unordered_set>
#include <functional>
#include <memory>

#include "RenderTarget.h"
#include "RenderTargetConfig.h"
#include "../LoadedTexture.h"
#include "../renderable/Renderable.h"
#include "../../scene/Referenceable.h"

class Engine;
class Model;
class Camera;
class ShaderProgram;

class RenderTarget : public Renderable
{
public:
	using ControllerFunction = std::function<void(std::vector<Model*> model_pool)>;

private:
	GLuint m_fbo = NULL;
	bool m_fbo_contains_render = false;
	GLenum m_fbo_target_type = GL_TEXTURE_2D;

	Camera* m_camera = nullptr;
	Engine* m_engine = nullptr;

	std::unique_ptr<ShaderProgram> m_shader_program = nullptr;

	ControllerFunction m_render_function;

	std::unique_ptr<Model> m_postprocess_model = nullptr;

protected:
	//scene rendering
	RenderTargetConfig m_config;

	void SetFramebuffer(GLuint fbo);
	GLuint GetFramebuffer() const;
	void SetTargetType(GLenum target_type);

	virtual void PreRenderEvent(); //happens just before rendering
	virtual void PostRenderEvent(); //happens just after rendering (deferred to before the next render when continuous_draw = true

	static bool RenderModeIsModelRendering(RenderTargetMode mode);
	bool RenderModeIsModelRendering();
	bool RenderModeIsFSQuadRendering();

	//rendering stages
	std::vector<Model*> Render_GetModels_Model(std::vector<Model*> model_pool);
	void Render_Setup_Model(std::vector<Model*> models);
	void Render_Setup_FlatQuad();
	void Render_ForEachModel_Model(Model* model);
	void Render_ForEachModel_Quad(Model* model);

public:
	RenderTarget(Engine* engine, RenderTargetConfig config);
	virtual ~RenderTarget();
	
	void SetCamera(Camera* camera);
	Camera* GetCamera() const;

	Engine* GetEngine() const;

	GLenum GetTargetType() const;

	void SetConfig(RenderTargetConfig config);
	void SetModeConfig(RenderTargetConfig::Normal_DepthOnly mode_config);
	void SetModeConfig(RenderTargetConfig::Normal_Draw mode_config);
	void SetModeConfig(RenderTargetConfig::Wireframe mode_config);
	void SetModeConfig(RenderTargetConfig::Shadow mode_config);
	void SetModeConfig(RenderTargetConfig::PostProcess mode_config);
	void SetModeConfig(RenderTargetConfig::Textured mode_config);
	RenderTargetConfig GetConfig() const;
	RenderTargetMode GetRenderMode() const;

	bool FramebufferContainsRenderOutput() const;
	bool IsFBOClearedOnRender() const;

	void Render(std::vector<Model*> models, bool continuous_draw = false) override;
	void RenderScene(std::vector<Model*> models); //only for calling by lambdas passed in through SetRenderFunction

	virtual bool SwapBuffers() = 0;

	void SetRenderFunction(ControllerFunction function);

	std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const;

	virtual void CopyFrom(const RenderTarget* src) const;
	void CopyTo(const RenderTarget* dest) const;
};
