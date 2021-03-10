#pragma once

#include "../../../GLComponents.h"

#include <GL/GL.h>

#include <vector>
#include <unordered_set>
#include <functional>
#include <memory>
#include <array>

#include "RenderTargetConfig.h"
#include "../../renderable/Renderable.h"
#include "../../../scene/Referenceable.h"
#include "../../TargetType.h"

class Engine;
class Model;
class Camera;
class ShaderProgram;

class RenderTarget : public Renderable
{
public:
	using ControllerFunction = std::function<void(std::vector<Model*> model_pool)>;

private:
	GLuint m_fbo = 0;
	bool m_fbo_contains_render = false;
	TargetType m_fbo_target = TargetType::Texture_2D;
	
	bool m_last_draw_was_continuous;

	Camera* m_camera = nullptr;
	Engine* m_engine = nullptr;

	std::unique_ptr<ShaderProgram> m_shader_program = nullptr;

	ControllerFunction m_render_function;

	std::unique_ptr<Model> m_postprocess_model = nullptr;

	std::array<glm::mat4, 6> m_cubemap_rotations;

	void CheckParentContext() const;

protected:
	//scene rendering
	RenderTargetConfig m_config;

	void SetFramebuffer(GLuint fbo);
	GLuint GetFramebuffer() const;
	void SetTargetType(TargetType target_type);

	virtual void PreRenderEvent(); //happens just before rendering
	virtual void PostRenderEvent(); //happens just after rendering (deferred to before the next render when continuous_draw = true

	bool RenderModeIsModelRendering() const;
	bool RenderModeIsFSQuadRendering() const;

	//rendering stages
	std::vector<Model*> Render_GetModels_Model(std::vector<Model*> model_pool);
	void Render_Setup_Model(std::vector<Model*> models);
	void Render_Setup_FSQuad();
	void Render_ForEachModel_Model(Model* model);
	void Render_ForEachModel_FSQuad(Model* model);

public:
	RenderTarget(Engine* engine, RenderTargetConfig config);
	virtual ~RenderTarget();
	
	void SetCamera(Camera* camera);
	Camera* GetCamera() const;

	Engine* GetEngine() const;

	TargetType GetTargetType() const;

	void SetConfig(RenderTargetConfig config);
	void SetModeConfig(RenderTargetConfig::Normal_DepthOnly mode_config);
	void SetModeConfig(RenderTargetConfig::Normal_Draw mode_config);
	void SetModeConfig(RenderTargetConfig::Normal_SSRQuality mode_config);
	void SetModeConfig(RenderTargetConfig::Wireframe mode_config);
	void SetModeConfig(RenderTargetConfig::Shadow mode_config);
	void SetModeConfig(RenderTargetConfig::PostProcess mode_config);
	void SetModeConfig(RenderTargetConfig::Textured mode_config);
	const RenderTargetConfig& GetConfig() const;
	RenderTargetMode GetRenderMode() const;

	bool FramebufferContainsRenderOutput() const;
	bool IsFBOClearedOnRender() const;

	void Render(std::vector<Model*> models, bool continuous_draw = false) override;
	void RenderScene(std::vector<Model*> models); //only for calling by lambdas passed in through SetRenderFunction

	virtual bool SwapBuffers() = 0;

	void SetRenderFunction(ControllerFunction function);

	std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const;

	virtual void CopyFrom(const RenderTarget* src);
	void CopyTo(RenderTarget* dest) const;
};
