#include "Renderable.h"

void Renderable::RenderScene()
{
	Scene* scene = this->m_engine->GetScene();
	if (scene != nullptr)
	{
		//set preprocessor defines

		if (this->m_rendermode == RenderMode::Normal)
		{
			//draw shadows and reflections (if required)
		}

		glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);
		glCullFace(GL_BACK);
		glViewport(0, 0, std::get<0>(this->GetOutputSize()), std::get<1>(this->GetOutputSize()));
		glClearColor(
			scene->GetClearColour().r,
			scene->GetClearColour().g,
			scene->GetClearColour().b,
			scene->GetClearColour().a
		);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		//load camera data into shader program

		std::vector<Model*> models_to_draw = scene->GetVisibleModels(this->m_camera->GetPosition(), this->m_rendermode);

		//iterate through models
		// load model data into shader program
		// load model textures into shader program
		// draw model
	}
}

void Renderable::RecompileShader()
{
	if (this->m_shader_program == nullptr)
	{
		delete this->m_shader_program;
	}

	this->m_shader_program = new ShaderProgram(
		this->m_shaders,
		this->m_shader_defines,
		false
	);

	for (std::vector<std::string>::iterator it = this->m_shader_uniform_names.begin(); it != this->m_shader_uniform_names.end(); it++)
	{
		this->m_shader_program->RegisterUniform(*it);
	}
}

void Renderable::SetFramebuffer(GLuint fbo)
{
	this->m_fbo = fbo;
}

void Renderable::PreRenderEvent()
{
}

void Renderable::PostRenderEvent()
{

}

Renderable::Renderable(Engine* engine, std::vector<std::tuple<std::string, GLenum>> shaders)
{
	this->m_engine = engine;
	this->m_shaders = shaders;

	this->RecompileShader();
}

Renderable::~Renderable()
{
	delete this->m_shader_program;
}

void Renderable::SetCamera(Camera* camera)
{
	this->m_camera = camera;
}

Camera* Renderable::GetCamera()
{
	return this->m_camera;
}

Engine* Renderable::GetEngine()
{
	return this->m_engine;
}

void Renderable::Render(bool continuous_draw)
{
	if (this->m_engine->GetScene() != nullptr)
	{
		if (continuous_draw)
		{
			this->PostRenderEvent();
		}
		this->PreRenderEvent();

		std::tuple<int, int> sizes = this->GetOutputSize();

		glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);
		glViewport(0, 0, std::get<0>(sizes), std::get<0>(sizes));
		this->RenderScene();

		if (!continuous_draw)
		{
			this->PostRenderEvent();
		}
	}
}

std::tuple<int, int> Renderable::GetOutputSize()
{
	throw std::logic_error("Method must be overridden");
}


void Renderable::SetRenderMode(RenderMode mode)
{
	this->m_rendermode = mode;
}

RenderMode Renderable::GetRenderMode()
{
	return this->m_rendermode;
}