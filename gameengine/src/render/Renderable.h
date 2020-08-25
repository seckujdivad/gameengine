#pragma once

const int ENGINECANVAS_NUM_DATA_TEX = 1;

#include "../GLComponents.h"

#include <GL/GL.h>

#include <vector>
#include <string>
#include <tuple>
#include <map>

#include "RenderMode.h"
#include "../Resource.h"
#include "../scene/model/Model.h"
#include "../scene/Camera.h"
#include "RenderTextureData.h"
#include "ShaderProgram.h"

class Engine;
class Model;
class Camera;

struct NormalRenderModeData
{
	RenderTextureGroup previous_frame;
};

struct WireframeRenderModeData
{

};

struct ShadowRenderModeData
{

};

struct PostProcessRenderModeData
{
	RenderTextureGroup texture;
};

class Renderable
{
private:
	GLuint m_fbo = -1;
	bool m_fbo_contains_render = false;
	GLenum m_fbo_target_type = GL_TEXTURE_2D;

	//scene rendering
	RenderMode m_rendermode = RenderMode::Default;

	NormalRenderModeData m_rendermode_data_normal;
	WireframeRenderModeData m_rendermode_data_wireframe;
	ShadowRenderModeData m_rendermode_data_shadow;
	PostProcessRenderModeData m_rendermode_data_postprocess;

	Camera* m_camera = nullptr;
	Engine* m_engine = nullptr;

	GLuint m_vao;
	std::map<ModelReference, GLuint> m_vbos;

	ShaderProgram* m_shader_program = nullptr;
	std::vector<std::tuple<std::string, GLenum>> m_shaders;
	std::map<std::string, std::string> m_shader_defines;
	std::vector<std::string> m_shader_uniform_names;

	std::map<int, LoadedTexture> m_textures;

	void RenderScene(std::vector<Model*> models = { nullptr });
	void RecompileShader();

protected:
	void SetFramebuffer(GLuint fbo);
	GLuint GetFramebuffer();
	void SetTargetType(GLenum target_type);

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

	void ConfigureShader(RenderMode mode);

	bool FramebufferContainsRenderOutput();

	virtual void PreRenderEvent(); //happens just before rendering
	virtual void PostRenderEvent(); //happens just after rendering (deferred to before the next render when continuous_draw = true

public:
	Renderable(Engine* engine, RenderMode mode);
	Renderable(const Renderable& copy_from) = delete;
	Renderable& operator=(const Renderable& copy_from) = delete;
	virtual ~Renderable();
	
	void SetCamera(Camera* camera);
	Camera* GetCamera();

	Engine* GetEngine();

	void SetRenderMode(NormalRenderModeData data);
	void SetRenderMode(WireframeRenderModeData data);
	void SetRenderMode(ShadowRenderModeData data);
	void SetRenderMode(PostProcessRenderModeData data);
	RenderMode GetRenderMode();

	virtual void Render(std::vector<Model*> models = { nullptr }, bool continuous_draw = false);
	virtual std::tuple<int, int> GetOutputSize();
};

#include "../Engine.h"