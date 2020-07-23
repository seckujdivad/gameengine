#pragma once

const int ENGINECANVAS_NUM_DATA_TEX = 1;

#include "../GLComponents.h"

#include <GL/GL.h>

#include <vector>
#include <string>
#include <tuple>
#include <map>

#include "../scene/Camera.h"
#include "../Resource.h"
#include "ShaderProgram.h"
#include "../Engine.h"

class Scene;
class Camera;

enum class RenderMode
{
	Normal,
	Editor
};

class Renderable
{
private:
	GLuint m_fbo = -1;

	//scene rendering
	RenderMode m_rendermode = RenderMode::Normal;

	Camera* m_camera = nullptr;
	Engine* m_engine = nullptr;

	ShaderProgram* m_shader_program = nullptr;
	std::vector<std::tuple<std::string, GLenum>> m_shaders;
	std::vector<std::tuple<std::string, std::string>> m_shader_defines = {};
	std::vector<std::string> m_shader_uniform_names;

	std::map<int, LoadedTexture> m_textures;

	void RenderScene();
	void RecompileShader();

protected:
	void SetFramebuffer(GLuint fbo);

	virtual void PreRenderEvent(); //happens just before rendering
	virtual void PostRenderEvent(); //happens just after rendering (deferred to before the next render when continuous_draw = true

public:
	Renderable(Engine* engine, std::vector<std::tuple<std::string, GLenum>> shaders);
	~Renderable();
	
	void SetCamera(Camera* camera);
	Camera* GetCamera();

	Engine* GetEngine();

	void SetRenderMode(RenderMode mode);
	RenderMode GetRenderMode();

	virtual void Render(bool continuous_draw = false);
	virtual std::tuple<int, int> GetOutputSize();
};