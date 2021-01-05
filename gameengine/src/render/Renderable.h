#pragma once

#include "../GLComponents.h"

#include <GL/GL.h>
#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <vector>
#include <string>
#include <tuple>
#include <map>
#include <functional>
#include <unordered_set>
#include <memory>

#include "../scene/Referenceable.h"
#include "RenderMode.h"
#include "LoadedTexture.h"
#include "RenderableConfig.h"

class Engine;
class Model;
class Camera;
class ShaderProgram;
class Renderable;

class Renderable
{
public:
	using ControllerFunction = std::function<void(std::vector<Model*>)>;

private:
	GLuint m_fbo = -1;
	bool m_fbo_contains_render = false;
	GLenum m_fbo_target_type = GL_TEXTURE_2D;

	Camera* m_camera = nullptr;
	Engine* m_engine = nullptr;

	std::unique_ptr<ShaderProgram> m_shader_program = nullptr;

	ControllerFunction m_render_function;

	std::unique_ptr<Model> m_postprocess_model = nullptr;

protected:
	//scene rendering
	RenderableConfig m_config;

	void SetFramebuffer(GLuint fbo);
	GLuint GetFramebuffer() const;
	void SetTargetType(GLenum target_type);
	GLenum GetTargetType() const;

	virtual void PreRenderEvent(); //happens just before rendering
	virtual void PostRenderEvent(); //happens just after rendering (deferred to before the next render when continuous_draw = true

	static bool RenderModeIsModelRendering(RenderMode mode);
	bool RenderModeIsModelRendering();

public:
	Renderable(Engine* engine, RenderableConfig config);
	virtual ~Renderable();
	
	void SetCamera(Camera* camera);
	Camera* GetCamera() const;

	Engine* GetEngine() const;

	void SetConfig(RenderableConfig config);
	RenderableConfig GetConfig() const;
	RenderMode GetRenderMode() const;

	bool FramebufferContainsRenderOutput() const;

	void Render(std::vector<Model*> models = { nullptr }, bool continuous_draw = false);
	void RenderScene(std::vector<Model*> models = { nullptr }); //only for calling by lambdas passed in through SetRenderFunction

	virtual std::tuple<int, int> GetOutputSize() const = 0;

	void SetRenderFunction(ControllerFunction function);

	std::unordered_set<RenderTextureReference> GetRenderTextureDependencies() const;
};
