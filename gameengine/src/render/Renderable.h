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
#include <set>

#include "../scene/Referenceable.h"
#include "RenderMode.h"
#include "LoadedTexture.h"
#include "RenderableConfig.h"

class Engine;
class Model;
class Camera;
class ShaderProgram;
class Renderable;

using RenderableControllerFunction = std::function<void(std::vector<Model*>)>;

class Renderable
{
private:
	GLuint m_fbo = -1;
	bool m_fbo_contains_render = false;
	GLenum m_fbo_target_type = GL_TEXTURE_2D;

	Camera* m_camera = nullptr;
	Engine* m_engine = nullptr;

	GLuint m_vao;
	std::map<ModelReference, GLuint> m_vbos;

	ShaderProgram* m_shader_program = nullptr;
	std::vector<std::tuple<std::string, GLenum>> m_shaders;
	std::map<std::string, std::string> m_shader_defines;
	std::set<std::string> m_shader_uniform_names;

	std::map<int, LoadedTexture> m_textures;

	RenderableControllerFunction m_render_function;

	void RecompileShader();

protected:
	//scene rendering
	RenderableConfig m_config;

	void SetFramebuffer(GLuint fbo);
	GLuint GetFramebuffer() const;
	void SetTargetType(GLenum target_type);
	GLenum GetTargetType() const;

	bool SetShaderDefine(std::string key, std::string value); //returns whether or not the shader requires recompilation (this is deferred to the caller)
	void AddShaderUniformName(std::string name);
	void AddShaderUniformNames(std::vector<std::string> names);

	void SetShaderUniform(std::string name, bool value);
	void SetShaderUniform(std::string name, int value);
	void SetShaderUniform(std::string name, float value);
	void SetShaderUniform(std::string name, double value, bool demote = true);
	void SetShaderUniform(std::string name, glm::vec3 vec);
	void SetShaderUniform(std::string name, glm::dvec3 vec, bool demote = true);
	void SetShaderUniform(std::string name, glm::vec4 vec);
	void SetShaderUniform(std::string name, glm::dvec4 vec, bool demote = true);
	void SetShaderUniform(std::string name, glm::mat4 mat);
	void SetShaderUniform(std::string name, glm::dmat4 mat, bool demote = true);
	void SetShaderUniform(std::string name, glm::mat3 mat);
	void SetShaderUniform(std::string name, glm::dmat3 mat, bool demote = true);

	virtual void PreRenderEvent(); //happens just before rendering
	virtual void PostRenderEvent(); //happens just after rendering (deferred to before the next render when continuous_draw = true

	static bool RenderModeIsModelRendering(RenderMode mode);
	bool RenderModeIsModelRendering();

public:
	Renderable(Engine* engine, RenderableConfig config);
	Renderable(const Renderable&) = delete;
	Renderable& operator=(const Renderable&) = delete;
	Renderable(Renderable&&) = delete;
	Renderable& operator=(Renderable&&) = delete;
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

	void SetRenderFunction(RenderableControllerFunction function);
};
