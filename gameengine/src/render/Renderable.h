#pragma once

const int ENGINECANVAS_NUM_DATA_TEX = 1;

#include "../GLComponents.h"

#include <GL/GL.h>

#include <vector>
#include <string>
#include <tuple>
#include <map>

#include "../scene/Camera.h"
#include "../scene/Scene.h"
#include "../Resource.h"
#include "ShaderProgram.h"

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

	Camera* m_active_camera = nullptr;
	Scene* m_scene = nullptr;

	ShaderProgram* m_shader_program = nullptr;
	std::vector<std::tuple<std::string, std::string>> m_shader_defines = {};
	std::string m_shader_fragment;
	std::string m_shader_vertex;

	std::map<int, LoadedTexture> m_textures;

	void RenderScene();
	void RecompileShader();

	//post processing
	int m_old_size[2] = { 1, 1 };

	ShaderProgram* m_postprocessor = nullptr;
	GLuint m_postprocessor_fbo = NULL;
	GLuint m_postprocessor_vao = NULL;
	GLuint m_postprocessor_vbo = NULL;

	GLuint m_postprocessor_depth_texture_write = NULL;
	GLuint m_postprocessor_colour_texture_write = NULL;
	std::vector<GLuint> m_postprocessor_data_textures_write;

	GLuint m_postprocessor_depth_texture_read = NULL;
	GLuint m_postprocessor_colour_texture_read = NULL;
	std::vector<GLuint> m_postprocessor_data_textures_read;

protected:
	void SetFramebuffer(GLuint fbo);

	virtual void RenderInitialisationEvent(); //happens before rendering
	virtual void PreRenderEvent(); //happens just before rendering, or just after when continuous draw is true
	virtual void PostRenderEvent(); //happens just after rendering, or just before when continuous draw is true

public:
	Renderable(Scene* scene, std::string vert_shader, std::string frag_shader);
	~Renderable();

	Scene* GetScene();
	void SetActiveCamera(Camera* camera);
	Camera* GetActiveCamera();

	void Render(bool continuous_draw = false);

	virtual std::tuple<int, int> GetOutputSize();

	void SetPostProcessorShaderProgram(ShaderProgram* postprocessor);

	void SetRenderMode(RenderMode mode);
	RenderMode GetRenderMode();

	void SetShaderPreprocessorDefine(std::string name, std::string value);
};