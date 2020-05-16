#pragma once

const int ENGINECANVAS_NUM_DATA_TEX = 1;

#include "../GLComponents.h"
#include "../scene/Camera.h"
#include "../scene/Scene.h"

class Scene;
class Camera;

class Renderable
{
private:
	Camera* m_active_camera = nullptr;
	GLuint m_fbo = -1;

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
	Scene* m_scene = nullptr;

	void SetFramebuffer(GLuint fbo);

	virtual void PostRenderEvent();

public:
	Renderable();
	~Renderable();

	void SetScene(Scene* scene);
	Scene* GetScene();
	void SetActiveCamera(Camera* camera);
	Camera* GetActiveCamera();

	void Render(bool continuous_draw = false);

	virtual std::tuple<int, int> GetOutputSize();

	void SetPostProcessorShaderProgram(ShaderProgram* postprocessor);
};